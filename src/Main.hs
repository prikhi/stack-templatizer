{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           Data.ByteString.Builder        ( Builder
                                                , byteString
                                                , stringUtf8
                                                , toLazyByteString
                                                )
import           Data.List                      ( intersperse
                                                , sort
                                                )
import           Control.Exception              ( tryJust )
import           Control.Monad                  ( guard )
import           Data.Maybe                     ( mapMaybe )
import           Data.Text.Encoding             ( decodeUtf8Lenient )
import           Ignore                         ( Ignore
                                                , ignores'
                                                , parse
                                                )
import           System.Directory               ( listDirectory
                                                , doesDirectoryExist
                                                , doesFileExist
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )
import           System.FilePath                ( (</>) )
import           System.IO.Error                ( isDoesNotExistError )
import           System.OsPath                  ( OsPath
                                                , encodeFS
                                                )

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base64        as Base64
import qualified Data.ByteString.Lazy          as LBS

main :: IO ()
main = getArgs >>= \case
    [folderName] -> templatize folderName
        >>= LBS.writeFile (folderName ++ ".hsfiles") . toLazyByteString
    _ -> printHelp >> exitFailure


printHelp :: IO ()
printHelp = mapM_
    putStrLn
    [ "stack-templatizer: Generate Stack Templates from a Folder"
    , ""
    , "Usage: stack-templatizer FOLDER_NAME"
    , ""
    , "The generated file will be named `<folder-name>.hsfiles`"
    , "Files that are not valid UTF-8 are embedded base64-encoded."
    , "Files matched by .gitignore files are skipped, including nested"
    , "ones, with nearer .gitignore files taking precedence. If a"
    , "top-level .gitignore is present, `.git` is skipped as well."
    ]


templatize :: FilePath -> IO Builder
templatize folder = do
    mRootIgnore <- loadDirIgnore folder
    let ignoreStack = case mRootIgnore of
            Just rootIgnore ->
                let ig = rootIgnore <> parse ".git" in [(0, ig, globAll <> ig)]
            Nothing -> []
    fileNames        <- getFilesInDirectory ignoreStack folder
    namesAndContents <- mapM
        (\file -> (file, ) <$> BS.readFile (folder </> file))
        fileNames
    return $ generateHFiles namesAndContents


loadDirIgnore :: FilePath -> IO (Maybe Ignore)
loadDirIgnore dir = do
    let gitignorePath = dir </> ".gitignore"
    exists <- doesFileExist gitignorePath
    if exists
        then either (const Nothing) (Just . parse . decodeUtf8Lenient)
                <$> tryJust (guard . isDoesNotExistError)
                            (BS.readFile gitignorePath)
        else return Nothing


globAll :: Ignore
globAll = parse "*"


verdict :: Ignore -> Ignore -> [OsPath] -> Bool -> Maybe Bool
verdict ig igWithGlobAll path isDir
    | ignores' ig path isDir            = Just True
    | ignores' igWithGlobAll path isDir = Nothing
    | otherwise                         = Just False


isIgnored :: [(Int, Ignore, Ignore)] -> [OsPath] -> Bool -> Bool
isIgnored ignoreStack components isDir =
    case
            mapMaybe
                (\(depth, ig, igWithGlobAll) ->
                    verdict ig igWithGlobAll (drop depth components) isDir
                )
                ignoreStack
        of
            (v : _) -> v
            []      -> False


getFilesInDirectory :: [(Int, Ignore, Ignore)] -> FilePath -> IO [FilePath]
getFilesInDirectory rootIgnoreStack baseDirectory = do
    basePaths <- listDirSorted baseDirectory
    concat <$> mapM (recursiveList rootIgnoreStack 0 [] "") basePaths
  where
    listDirSorted :: FilePath -> IO [FilePath]
    listDirSorted =
        fmap sort . listDirectory
    recursiveList
        :: [(Int, Ignore, Ignore)]
        -> Int
        -> [OsPath]
        -> String
        -> FilePath
        -> IO [FilePath]
    recursiveList ignoreStack depth parentComponents parentDir path = do
        let templatePath = parentDir </> path
            fullPath     = baseDirectory </> templatePath
        component <- encodeFS path
        let components = parentComponents ++ [component]
            depth'      = depth + 1
        isDirectory <- doesDirectoryExist fullPath
        if isIgnored ignoreStack components isDirectory
            then return []
            else if isDirectory
                then do
                    mChildIgnore <- loadDirIgnore fullPath
                    let ignoreStack' = case mChildIgnore of
                            Just childIgnore ->
                                (depth', childIgnore, globAll <> childIgnore)
                                    : ignoreStack
                            Nothing -> ignoreStack
                    files <- listDirSorted fullPath
                    concat
                        <$> mapM
                                (recursiveList ignoreStack'
                                               depth'
                                               components
                                               templatePath
                                )
                                files
                else return [templatePath]


generateHFiles :: [(FilePath, BS.ByteString)] -> Builder
generateHFiles = mconcat . intersperse "\n" . map renderSection
  where
    renderSection :: (FilePath, BS.ByteString) -> Builder
    renderSection (file, contents)
        | BS.isValidUtf8 contents =
            "{-# START_FILE " <> stringUtf8 file <> " #-}\n"
                <> byteString contents
        | otherwise =
            "{-# START_FILE BASE64 " <> stringUtf8 file <> " #-}\n"
                <> foldMap ((<> "\n") . byteString)
                           (chunksOf 76 $ Base64.encode contents)


chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf size bytes
    | BS.null bytes = []
    | otherwise =
        let (chunk, rest) = BS.splitAt size bytes
        in  chunk : chunksOf size rest
