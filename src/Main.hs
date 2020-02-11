{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import           System.Directory               ( listDirectory
                                                , doesDirectoryExist
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )
import           System.FilePath                ( (</>) )

import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Char8    as LC

main :: IO ()
main = getArgs >>= \case
    [folderName] ->
        templatize folderName >>= LBS.writeFile (folderName ++ ".hsfiles")
    _ -> printHelp >> exitFailure


printHelp :: IO ()
printHelp = mapM_
    putStrLn
    [ "stack-templatizer: Generate Stack Templates from a Folder"
    , ""
    , "Usage: stack-templatizer FOLDER_NAME"
    , ""
    , "The generated file will be named `<folder-name>.hsfiles`"
    ]


templatize :: FilePath -> IO LBS.ByteString
templatize folder = do
    fileNames        <- getFilesInDirectory folder
    namesAndContents <- mapM (\file -> (file, ) <$> LBS.readFile file) fileNames
    return $ generateHFiles namesAndContents


getFilesInDirectory :: FilePath -> IO [FilePath]
getFilesInDirectory = recursiveList ""
  where
    recursiveList :: String -> FilePath -> IO [FilePath]
    recursiveList parentDir path = do
        let fullPath = parentDir </> path
        isDirectory <- doesDirectoryExist fullPath
        if isDirectory
            then do
                files <- listDirectory fullPath
                concat <$> mapM (recursiveList fullPath) files
            else return [fullPath]


generateHFiles :: [(FilePath, LBS.ByteString)] -> LBS.ByteString
generateHFiles = LBS.intercalate "\n\n" . map prefixName
  where
    prefixName :: (FilePath, LBS.ByteString) -> LBS.ByteString
    prefixName (file, contents) =
        "{-# START_FILE " <> LC.pack file <> " #-}\n" <> contents
