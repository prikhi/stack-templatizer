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
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8Lenient
                                                , encodeUtf8
                                                )
import           Ignore                         ( Ignore
                                                , ignores'
                                                , parse
                                                )
import           Options.Applicative            ( Parser
                                                , ParserInfo
                                                , ReadM
                                                , argument
                                                , eitherReader
                                                , execParser
                                                , fullDesc
                                                , header
                                                , footer
                                                , help
                                                , helper
                                                , info
                                                , long
                                                , metavar
                                                , option
                                                , progDesc
                                                , str
                                                , value
                                                , (<**>)
                                                )
import           System.Directory               ( listDirectory
                                                , doesDirectoryExist
                                                , doesFileExist
                                                )
import           System.FilePath                ( (</>) )
import           System.IO.Error                ( isDoesNotExistError )
import           System.OsPath                  ( OsPath
                                                , encodeFS
                                                )

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base64        as Base64
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text                     as Text

data Options = Options
    { optNameToken :: String
    , optFolder    :: FilePath
    }

main :: IO ()
main = do
    opts <- execParser optsInfo
    templatize (Text.pack $ optNameToken opts) (optFolder opts)
        >>= LBS.writeFile (optFolder opts ++ ".hsfiles") . toLazyByteString


optsInfo :: ParserInfo Options
optsInfo = info (optionsParser <**> helper) $ mconcat
    [ fullDesc
    , header "stack-templatizer - Generate Stack Templates from a Folder"
    , progDesc "Generate a Stack template from a folder"
    , footer
        (  "The generated file will be named `<folder-name>.hsfiles`. "
        <> "Files that are not valid UTF-8 are embedded base64-encoded. "
        <> "Files matched by .gitignore files are skipped, including "
        <> "nested ones, with nearer .gitignore files taking precedence. "
        <> "If a top-level .gitignore is present, `.git` is skipped as "
        <> "well. Occurrences of the name token in file names and "
        <> "UTF-8 file contents are replaced with `{{name}}`."
        )
    ]


optionsParser :: Parser Options
optionsParser =
    Options
        <$> option
                nameTokenReader
                (  long "name-token"
                <> metavar "TOKEN"
                <> value "PACKAGENAME"
                <> help
                       (  "Token in file names & UTF-8 contents to replace "
                       <> "with `{{name}}` (default: PACKAGENAME)"
                       )
                )
        <*> argument str (metavar "FOLDER_NAME")


nameTokenReader :: ReadM String
nameTokenReader = eitherReader $ \s -> if null s
    then Left "--name-token must not be empty"
    else Right s


templatize :: Text -> FilePath -> IO Builder
templatize nameToken folder = do
    mRootIgnore <- loadDirIgnore folder
    let ignoreStack = case mRootIgnore of
            Just rootIgnore ->
                let ig = rootIgnore <> parse ".git" in [(0, ig, globAll <> ig)]
            Nothing -> []
    fileNames        <- getFilesInDirectory ignoreStack folder
    namesAndContents <- mapM
        (\file -> (file, ) <$> BS.readFile (folder </> file))
        fileNames
    return $ generateHFiles $ map (substituteToken nameToken) namesAndContents


substituteToken :: Text -> (FilePath, BS.ByteString) -> (FilePath, BS.ByteString)
substituteToken token (file, contents) =
    ( Text.unpack (replaceToken (Text.pack file))
    , if BS.isValidUtf8 contents
        then encodeUtf8 (replaceToken (decodeUtf8Lenient contents))
        else contents
    )
    where replaceToken = Text.replace token "{{name}}"


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
