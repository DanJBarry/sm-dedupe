module Main where

import           Prelude                 hiding ( lookup )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import           Data.Digest.Pure.SHA
import           SmDedupe
import           SmDedupe.Parse
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.Parsec.Text.Lazy

-- | Go through given paths recursively, find .sm files, get their chartkeys, check for
-- duplicates, and replace duplicates with a symlink
dedupe :: [FilePath] -> Map (Set (Digest SHA1State)) [Char] -> IO [Char]
dedupe [] songs = do
  putStrLn "Showing chartkeys"
  return
    $ foldl
        (\acc file ->
          foldl (\acc2 digest -> acc2 ++ " " ++ showDigest digest) acc file
        )
        "Chartkeys:"
    $ Map.keys songs
dedupe (path : paths) songs = do
  isFile <- doesFileExist path
  if isExtensionOf "sm" path && isFile
    then do
      parseResult <- parseFromFile parseSm path
      case parseResult of
        Left error -> return $ show error
        Right steps ->
          let chartkeys = getChartkeys steps
          in  let parent = takeDirectory path
              in  case Map.lookup chartkeys songs of
                    Just target -> do
                      putStrLn
                        $  "Found duplicate of "
                        ++ target
                        ++ " in "
                        ++ parent
                      putStrLn $ "Creating symlink..."
                      removeDirectoryRecursive parent
                      createDirectoryLink target parent
                      dedupe paths songs
                    Nothing -> dedupe paths $ Map.insert chartkeys parent songs
    else do
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then do
          contents <- getDirectoryContents path
          dedupe
            ((map (\content -> path ++ "/" ++ content) $ filter notDot contents)
            ++ paths
            )
            songs
        else dedupe paths songs

main :: IO ()
main = do
  args <- getArgs
  setCurrentDirectory $ head args
  paths  <- getDirectoryContents "."
  result <- dedupe (filter notDot paths) Map.empty
  putStrLn result
