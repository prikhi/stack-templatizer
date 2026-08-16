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
import           System.Directory               ( listDirectory
                                                , doesDirectoryExist
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )
import           System.FilePath                ( (</>) )

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
    ]


templatize :: FilePath -> IO Builder
templatize folder = do
    fileNames        <- getFilesInDirectory folder
    namesAndContents <- mapM
        (\file -> (file, ) <$> BS.readFile (folder </> file))
        fileNames
    return $ generateHFiles namesAndContents


getFilesInDirectory :: FilePath -> IO [FilePath]
getFilesInDirectory baseDirectory = do
    basePaths <- listDirSorted baseDirectory
    concat <$> mapM (recursiveList "") basePaths
  where
    listDirSorted :: FilePath -> IO [FilePath]
    listDirSorted =
        fmap sort . listDirectory
    recursiveList :: String -> FilePath -> IO [FilePath]
    recursiveList parentDir path = do
        let templatePath = parentDir </> path
            fullPath     = baseDirectory </> templatePath
        isDirectory <- doesDirectoryExist fullPath
        if isDirectory
            then do
                files <- listDirSorted fullPath
                concat <$> mapM (recursiveList templatePath) files
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
