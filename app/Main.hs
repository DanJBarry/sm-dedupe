#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Prelude                 hiding ( lookup )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import           System.Directory               ( removeDirectoryLink )
import           System.FilePath                ( dropTrailingPathSeparator )
import           Data.Digest.Pure.SHA
import           Path
import           Path.IO
import           SmDedupe
import           SmDedupe.Parse
import           System.Console.CmdArgs
import           System.Environment
import           Text.Parsec.ByteString

data Opts = Opts {directory :: FilePath, unlink :: Bool}
              deriving (Show, Data, Typeable)

opts :: Opts
opts =
  Opts
      { directory = def &= args &= typ "DIR"
      , unlink    = def
        &= help "Undo symlinks and copy over their target directories"
      }
    &= program "sm-dedupe"
    &= summary "sm-dedupe v0.1.0, (C) Daniel Barry"
    &= help "A tool to remove duplicates from your Stepmania songs folder"

-- | Go through given paths recursively, find .sm files, get their chartkeys, check for
-- duplicates, and replace duplicates with a symlink
dedupe
  :: (Show a, Num a)
  => Map (Set (Digest SHA1State)) (Path Abs Dir)
  -> a
  -> [Path Abs Dir]
  -> [Path Abs File]
  -> IO [Char]
dedupe songs affected [] [] =
  return $ "Replaced " ++ show affected ++ " new duplicate(s) with symlinks"
dedupe songs affected (currentDirectory : directories) [] = do
  symlink <- isSymlink currentDirectory
  if symlink
    then dedupe songs affected directories []
    else do
      (newDirectories, newFiles) <- listDir currentDirectory
      dedupe songs affected (directories ++ newDirectories) newFiles
dedupe songs affected directories (currentFile : files) = do
  case fileExtension currentFile of
    Right ".sm" -> do
      parseResult <- parseFromFile parseSm $ toFilePath currentFile
      case parseResult of
        Left parseError -> return $ show parseError
        Right steps ->
          let chartkeys       = getChartkeys steps
              parentDirectory = parent currentFile
          in  case Map.lookup chartkeys songs of
                Just target -> do
                  putStrLn
                    $  "Found duplicate of "
                    ++ toFilePath target
                    ++ " in "
                    ++ toFilePath parentDirectory
                  putStrLn "Creating symlink..."
                  removeDirRecur parentDirectory
                  createDirLink target parentDirectory
                  dedupe songs (affected + 1) directories files
                Nothing -> dedupe
                  (Map.insert chartkeys parentDirectory songs)
                  affected
                  directories
                  files
    _ -> dedupe songs affected directories files

undo :: (Show t, Num t) => t -> [Path Abs Dir] -> IO [Char]
undo affected [] = return $ "Unlinked " ++ show affected ++ " directories"
undo affected (currentDirectory : directories) = do
  symlink <- isSymlink currentDirectory
  if symlink
    then do
      targetPath <- getSymlinkTarget currentDirectory
      case parseAbsDir targetPath of
        Right target -> do
          putStrLn
            $  "Unlinking "
            ++ show currentDirectory
            ++ " -> "
            ++ show target
          removeDirectoryLink $ dropTrailingPathSeparator $ toFilePath
            currentDirectory
          copyDirRecur target currentDirectory
          (newDirectories, _) <- listDir currentDirectory
          undo (affected + 1) $ directories ++ newDirectories
        _ -> undo affected directories
    else do
      (newDirectories, _) <- listDir currentDirectory
      undo affected $ directories ++ newDirectories

main :: IO ()
main = do
  parsedOpts  <- cmdArgs opts
  parsedDir   <- parseRelDir $ directory parsedOpts
  absoluteDir <- makeAbsolute parsedDir
  result      <- if unlink parsedOpts
    then undo 0 [absoluteDir]
    else dedupe Map.empty 0 [absoluteDir] []
  putStrLn result
