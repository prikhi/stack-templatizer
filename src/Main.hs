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
    namesAndContents <- mapM
        (\file -> (file, ) <$> LBS.readFile (folder </> file))
        fileNames
    return $ generateHFiles namesAndContents


getFilesInDirectory :: FilePath -> IO [FilePath]
getFilesInDirectory baseDirectory = do
    basePaths <- listDirectory baseDirectory
    concat <$> mapM (recursiveList "") basePaths
  where
    recursiveList :: String -> FilePath -> IO [FilePath]
    recursiveList parentDir path = do
        let templatePath = parentDir </> path
            fullPath     = baseDirectory </> templatePath
        isDirectory <- doesDirectoryExist fullPath
        if isDirectory
            then do
                files <- listDirectory fullPath
                concat <$> mapM (recursiveList templatePath) files
            else return [templatePath]


generateHFiles :: [(FilePath, LBS.ByteString)] -> LBS.ByteString
generateHFiles = LBS.intercalate "\n" . map prefixName
  where
    prefixName :: (FilePath, LBS.ByteString) -> LBS.ByteString
    prefixName (file, contents) =
        "{-# START_FILE " <> LC.pack file <> " #-}\n" <> contents
