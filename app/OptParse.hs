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
    <> help "The starting directory." 
    <> value "." 
    <> showDefault )

pAll :: Parser Bool
pAll =
  switch
    (  short 'a'
    <> help "Include files and directories including hidden files." )

pDepth :: Parser Natural
pDepth =
  option auto
    (  long "depth"
    <> short 'd'
    <> metavar "NATURAL"
    <> help "Descend only depth directories deep."
    <> value 50000 -- value that should cover all cases
    )
