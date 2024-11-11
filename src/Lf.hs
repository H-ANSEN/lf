module Lf ( printDir ) where

import Numeric.Natural
import Control.Monad (when)
import System.Posix (getFileStatus, isDirectory)
import System.Directory (listDirectory)
import System.FilePath ( (</>) )

entryStr, terminalStr, segmentStr, emptyStr :: String
entryStr    = "├── "
terminalStr = "└── "
segmentStr  = "│   "
emptyStr    = "    "

startsWithC :: Char -> String -> Bool
startsWithC _ [] = False
startsWithC c (x:_) = x == c

listDir :: Bool -> FilePath -> IO [FilePath]
listDir dotFiles dir = do 
  entries <- listDirectory dir
  if dotFiles then pure entries
              else pure $ filter (not . startsWithC '.') entries

isDir :: FilePath -> IO Bool
isDir path = isDirectory <$> getFileStatus path

walk :: Natural -> FilePath -> String -> Bool -> IO ()
walk depth current prefix dot = do
  when (depth > 0) $ do
    entries <- listDir dot current
    printEntries current entries
      where
        printEntries _ [] = pure ()
        printEntries parent (entry:rest) = do
          let isLastEntry = null rest
              newPrefix   = prefix <> if isLastEntry then emptyStr else segmentStr
              entryPrefix = prefix <> if isLastEntry then terminalStr else entryStr

          putStrLn (entryPrefix <> entry)
          let fullPath = parent </> entry
          directory <- isDir fullPath
          when directory $ walk (depth - 1) fullPath newPrefix dot
          printEntries parent rest

printDir :: Natural -> Bool -> FilePath -> IO ()
printDir depth dot dir = walk depth dir "" dot
