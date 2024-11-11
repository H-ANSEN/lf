module Main where

import qualified Lf

import OptParse
import System.Exit (exitFailure)
import System.Directory (doesPathExist)

main :: IO ()
main = do
  options <- parse
  exists <- doesPathExist $ startDir options
  if not exists 
    then exitFailure
    else
      let depth = maxDepth options
          dot   = includeDot options
          dir   = startDir options in
      Lf.printDir depth dot dir
