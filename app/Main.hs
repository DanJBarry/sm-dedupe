#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Prelude                 hiding ( lookup )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import           System.Directory               ( removeDirectoryLink
                                                , canonicalizePath
                                                )
import           System.FilePath                ( dropTrailingPathSeparator )
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Digest.Pure.SHA
import           Path
import           Path.IO
import           SmDedupe
import           SmDedupe.Parse
import           System.Console.CmdArgs
import           System.Environment
import           System.IO
import           Text.Parsec.ByteString
import           Text.Printf

data Opts =
  Opts
      { directories :: [FilePath]
      , exclude :: FilePath
      , force :: Bool
      , dryRun :: Bool
      , unlink :: Bool
      }
        deriving (Show, Data, Typeable)

type Songmap b = Map (Set (Digest SHA1State)) (Path b Dir)

opts :: Opts
opts =
  Opts
      { directories = def &= args &= typ "DIRS"
      , exclude     = def &= help "Directories to exclude" &= typ "DIRS"
      , force       =
        def &= help
          "Do not display a prompt before deleting or unlinking a directory"
      , dryRun      = def &= explicit &= name "d" &= name "dry-run" &= help
        "Do a dry run, printing out found duplicates without deleting them"
      , unlink      = def
        &= help "Undo symlinks and copy over their target directories"
      }
    &= program "sm-dedupe"
    &= summary "sm-dedupe v0.1.0, (C) Daniel Barry"
    &= help "A tool to remove duplicates from your Stepmania songs folder"

-- | Walk through dirs, accumulating a map of chartkeys to directories, and remove
-- any duplicate chartkeys, replacing their parent directory with a symlink, ignore
-- any directory in excludeDirs
dedupe
  :: (Traversable t1, Foldable t2)
  => Opts
  -> t2 (Path Abs Dir)
  -> t1 (Path b Dir)
  -> IO String
dedupe opts' excludeDirs dirs = do
  listDirs <- mapM listDirRecur dirs
  result   <-
    let files = concatMap (filter (smAndNotChildOf excludeDirs) . snd) listDirs
    in  foldM (dedupe' opts') Map.empty files
  return $ show (length result) ++ " unique songs"

-- | Get the chartkey of a file, return a new map with the included chartkey if the
-- chartkey does not exist yet, remove the directory and create a symlink if it does
-- exist
dedupe' :: Opts -> Songmap b1 -> Path b1 t -> IO (Songmap b1)
dedupe' opts' songs file = do
  parseResult <- parseFromFile parseSm $ toFilePath file
  case parseResult of
    Left parseError ->
      printf "Error parsing %s:\n%s\n" (toFilePath file) (show parseError)
        >> return songs
    Right steps ->
      let
        chartkeys       = getChartkeys steps
        parentDirectory = parent file
      in
        case Map.lookup chartkeys songs of
          Just target ->
            printf "Found duplicate of %s in %s\n"
                   (toFilePath target)
                   (toFilePath parentDirectory)
              >> if dryRun opts'
                   then return songs
                   else if force opts'
                     then makeSymlink target parentDirectory >> return songs
                     else
                       makeRelativeToCurrentDir parentDirectory
                       >>= printf "Remove %s? "
                       .   toFilePath
                       >>  hFlush stdout
                       >>  getLine
                       >>= yesNo
                             (makeSymlink target parentDirectory >> return songs
                             )
                             (return songs)
          Nothing -> return $ Map.insert chartkeys parentDirectory songs

-- Walk handler that ignores all arguments and excludes a constant list of directories
excludeHandler :: [Path b1 Dir] -> b2 -> b3 -> b4 -> IO (WalkAction b1)
excludeHandler = const . const . const . return . WalkExclude

-- | makeSymlink target parentDirectory removes parentDirectory and creates a symlink
-- to target in its place
makeSymlink :: Path b0 Dir -> Path b1 Dir -> IO ()
makeSymlink target parentDirectory =
  putStrLn "Creating symlink..."
    >> removeDirRecur parentDirectory
    >> createDirLink target parentDirectory

-- | Parse a string into an absolute path to a directory
parseDir :: MonadIO m => FilePath -> m (Path Abs Dir)
parseDir directory = case parseAbsDir directory of
  Right absoluteDir -> return absoluteDir
  _                 -> resolveDir' directory

-- | Walk through dirs, delete any directory symlinks, and copy the contents of the
-- symlink target to its location
undo :: Traversable t => Opts -> [Path Abs Dir] -> t (Path b Dir) -> IO String
undo opts' excludeDirs dirs = do
  result <- mapM
    (walkDirAccum (Just (excludeHandler excludeDirs))
                  (const . const . undo' opts')
    )
    dirs
  return $ "Unlinked " ++ show (sum (concat result)) ++ " directories"

-- | If dir is a symlink, remove the symlink and copy over the contents it pointed to
undo' :: Num a => Opts -> Path b1 Dir -> IO [a]
undo' opts' dir = do
  symlink <- isSymlink dir
  if symlink
    then do
      targetPath <- getSymlinkTarget dir
      case parseAbsDir targetPath of
        Right target ->
          printf "Found symlink: %s -> %s\n"
                 (toFilePath dir)
                 (toFilePath target)
            >> if dryRun opts'
                 then return []
                 else do
                   putStrLn "Copying files..."
                   removeDirectoryLink $ dropTrailingPathSeparator $ toFilePath
                     dir
                   copyDirRecur target dir
                   return [1]
        _ -> return []
    else return []

main :: IO ()
main = do
  parsedOpts  <- cmdArgs opts
  includeDirs <- mapM parseDir $ directories parsedOpts
  excludeDirs <- mapM parseDir $ splitCommas $ exclude parsedOpts
  result      <-
    let action          = if unlink parsedOpts then undo else dedupe
        filteredInclude = filterChildren includeDirs
    in  action parsedOpts excludeDirs filteredInclude
  putStrLn result
