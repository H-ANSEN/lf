module OptParse
  ( Options(..)
  , parse
  )
  where

import Numeric.Natural
import Options.Applicative

data Options = Options
  { maxDepth :: Natural
  , includeDot :: Bool
  , startDir :: FilePath
  }

parse :: IO Options
parse = execParser $
  info (pOptions <**> helper)
    ( fullDesc
      <> header "lf - a directory tree viewer"
      <> progDesc "List files and directories on the command line in a tree view" )

pOptions :: Parser Options
pOptions =
  Options <$> pDepth
          <*> pAll
          <*> pDir

pDir :: Parser FilePath
pDir =
  strArgument
    (  metavar "DIRECTORY"
    <> help "the starting directory" 
    <> value "." )

pAll :: Parser Bool
pAll =
  switch
    (  long "all"
    <> short 'a'
    <> help "include all hidden files (dotfiles)" )

pDepth :: Parser Natural
pDepth =
  option auto
    (  long "depth"
    <> short 'd'
    <> metavar "NATURAL"
    <> help "level of directories to recurse into"
    <> value 2
    )
