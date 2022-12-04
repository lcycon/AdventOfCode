module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Lib (runMain)
import System.Environment (getArgs)

main :: IO ()
main = do
  _ <- loadFile defaultConfig
  args <- getArgs
  runMain args
