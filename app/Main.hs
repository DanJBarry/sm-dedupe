module Main where

import           Prelude                 hiding ( lookup )
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import           Data.Digest.Pure.SHA
import           SmDedupe
import           SmDedupe.Parse
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Parsec.ByteString

-- | Go through given paths recursively, find .sm files, get their chartkeys, check for
-- duplicates, and replace duplicates with a symlink
dedupe
  :: (Show t, Num t)
  => Map (Set (Digest SHA1State)) String
  -> t
  -> [FilePath]
  -> IO String
dedupe _ symlinks [] =
  return $ "Replaced " ++ show symlinks ++ " new duplicate(s) with symlinks"
dedupe songs symlinks (path : paths) = do
  isFile        <- doesFileExist path
  canonicalPath <- canonicalizePath path
  relativePath  <- makeRelativeToCurrentDirectory canonicalPath
  if path == relativePath
    then if isExtensionOf "sm" canonicalPath && isFile
      then do
        parseResult <- parseFromFile parseSm canonicalPath
        case parseResult of
          Left parseError -> return $ show parseError
          Right steps ->
            let chartkeys = getChartkeys steps
                parent    = takeDirectory canonicalPath
            in  case Map.lookup chartkeys songs of
                  Just target -> do
                    putStrLn
                      $  "Found duplicate of "
                      ++ target
                      ++ " in "
                      ++ parent
                    putStrLn "Creating symlink..."
                    removeDirectoryRecursive parent
                    createDirectoryLink target parent
                    dedupe songs (symlinks + 1) paths
                  Nothing ->
                    dedupe (Map.insert chartkeys parent songs) symlinks paths
      else do
        isDirectory <- doesDirectoryExist path
        if isDirectory
          then do
            contents <- getDirectoryContents path
            dedupe songs symlinks
              $  map (path </>) (filter notDot contents)
              ++ paths
          else dedupe songs symlinks paths
    else dedupe songs symlinks paths

main :: IO ()
main = do
  (dir : _) <- getArgs
  setCurrentDirectory dir
  paths  <- getDirectoryContents "."
  result <- dedupe Map.empty 0 (filter notDot paths)
  putStrLn result
