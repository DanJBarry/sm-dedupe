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
import           Control.Monad                  ( liftM )

data Opts = Opts {directory :: FilePath, check :: Bool, pretend :: Bool, unlink :: Bool}
              deriving (Show, Data, Typeable)

opts :: Opts
opts =
  Opts
      { directory = def &= args &= typ "DIRS"
      , check     = def &= help
        "Only runs the parser, does not check chart keys or delete directories"
      , pretend   =
        def &= help
          "Do a dry run, printing out found duplicates without deleting them"
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
  => Opts
  -> Map (Set (Digest SHA1State)) (Path Abs Dir)
  -> a
  -> [Path Abs Dir]
  -> [Path Abs File]
  -> IO [Char]
dedupe opts songs affected [] []
  | check opts
  = return $ "Successfully parsed " ++ show affected ++ " .sm file(s)"
  | pretend opts
  = return $ "Found " ++ show affected ++ " duplicate(s)"
  | otherwise
  = return $ "Replaced " ++ show affected ++ " new duplicate(s) with symlinks"
dedupe opts songs affected (currentDirectory : directories) [] = do
  symlink <- isSymlink currentDirectory
  if symlink
    then dedupe opts songs affected directories []
    else do
      (newDirectories, newFiles) <- listDir currentDirectory
      dedupe opts songs affected (directories ++ newDirectories) newFiles
dedupe opts songs affected directories (currentFile : files) = do
  case fileExtension currentFile of
    Right ".sm" -> do
      parseResult <- parseFromFile parseSm $ toFilePath currentFile
      case parseResult of
        Left  parseError -> return $ show parseError
        Right steps      -> if check opts
          then dedupe opts songs (affected + 1) directories files
          else
            let chartkeys       = getChartkeys steps
                parentDirectory = parent currentFile
            in  case Map.lookup chartkeys songs of
                  Just target -> do
                    putStrLn
                      $  "Found duplicate of "
                      ++ toFilePath target
                      ++ " in "
                      ++ toFilePath parentDirectory
                    if pretend opts
                      then dedupe opts songs (affected + 1) directories files
                      else do
                        putStrLn "Creating symlink..."
                        removeDirRecur parentDirectory
                        createDirLink target parentDirectory
                        dedupe opts songs (affected + 1) directories files
                  Nothing -> dedupe
                    opts
                    (Map.insert chartkeys parentDirectory songs)
                    affected
                    directories
                    files
    _ -> dedupe opts songs affected directories files

undo :: (Show a, Num a) => Opts -> a -> [Path Abs Dir] -> IO [Char]
undo opts affected []
  | pretend opts = return $ "Found " ++ show affected ++ " symlinks"
  | otherwise    = return $ "Unlinked " ++ show affected ++ " directories"
undo opts affected (currentDirectory : directories) = do
  symlink <- isSymlink currentDirectory
  if symlink
    then do
      targetPath <- getSymlinkTarget currentDirectory
      case parseAbsDir targetPath of
        Right target -> do
          putStrLn
            $  "Found symlink: "
            ++ show currentDirectory
            ++ " -> "
            ++ show target
          if pretend opts
            then undo opts (affected + 1) directories
            else do
              putStrLn "Copying files..."
              removeDirectoryLink $ dropTrailingPathSeparator $ toFilePath
                currentDirectory
              copyDirRecur target currentDirectory
              (newDirectories, _) <- listDir currentDirectory
              undo opts (affected + 1) $ directories ++ newDirectories
        _ -> undo opts affected directories
    else do
      (newDirectories, _) <- listDir currentDirectory
      undo opts affected $ directories ++ newDirectories

main :: IO ()
main = do
  parsedOpts  <- cmdArgs opts
  parsedDir   <- parseRelDir $ directory parsedOpts
  absoluteDir <- makeAbsolute parsedDir
  result      <- if unlink parsedOpts
    then undo parsedOpts 0 [absoluteDir]
    else dedupe parsedOpts Map.empty 0 [absoluteDir] []
  putStrLn result
