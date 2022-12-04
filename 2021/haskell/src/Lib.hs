module Lib (runMain, debugAdvent) where

import Advent
  ( AoC (AoCInput, AoCSubmit),
    AoCOpts (AoCOpts),
    Day,
    Part (..),
    SubmitRes
      ( SubCorrect,
        SubIncorrect,
        SubInvalid,
        SubUnknown,
        SubWait
      ),
    mkDay,
    mkDay_,
    runAoC,
  )
import Common ( Impl, Text, parseDayPart )
import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import System.Console.Pretty
  ( Color (Green, Red, Yellow),
    Pretty (color),
  )
import System.Environment (getEnv)
import System.Exit (exitFailure)
import qualified Data.Map as M

impls :: M.Map (Day, Part) Impl
impls =
  M.fromList
    [ ((mkDay_ 1, Part1), Day1.part1),
      ((mkDay_ 1, Part2), Day1.part2),
      ((mkDay_ 2, Part1), Day2.part1),
      ((mkDay_ 2, Part2), Day2.part2),
      ((mkDay_ 3, Part1), Day3.part1),
      ((mkDay_ 3, Part2), Day3.part2),
      ((mkDay_ 4, Part1), Day4.part1),
      ((mkDay_ 4, Part2), Day4.part2)
    ]

runMain :: [String] -> IO ()
runMain [input] = runMainArg input
runMain [] = runMainAll
runMain _ = do
  putStrLn $ color Red "Invalid input"

runMainArg :: String -> IO ()
runMainArg input = do
  sessionKey <- getEnv "ADVENT_SESSION_KEY"
  let opts = AoCOpts sessionKey 2021 (Just "./cache") False 3000
  maybe (putStrLn $ color Red "Could not parse input") (uncurry (runAdvent opts)) (parseDayPart input)

runMainAll :: IO ()
runMainAll = do
  sessionKey <- getEnv "ADVENT_SESSION_KEY"
  let opts = AoCOpts sessionKey 2021 (Just "./cache") False 3000
  traverse_ (goRunAdvent opts) (M.keys impls)
  where
    goRunAdvent opts (day, part) = runAdvent opts day part

runAdvent :: AoCOpts -> Day -> Part -> IO ()
runAdvent opts day part = do
  input <- runAoC opts (AoCInput day)
  func <- maybe (putStrLn (color Red "Unknown impl") >> exitFailure) return $ M.lookup (day, part) impls
  let result = func (fromRight (error "Oh no! Couldn't get the input") input)
  Right (_, response) <- runAoC opts (AoCSubmit day part result)
  putStr $ "Day " ++ show day ++ ", " ++ show part ++ ": "
  putStrLn $ case response of
    SubCorrect _ -> color Green "CORRECT"
    SubIncorrect _ _ -> color Red "INCORRECT"
    SubWait n -> color Yellow $ mconcat ["Must wait ", show n, " seconds before submitting again"]
    SubInvalid -> "Submission to invalid question: " ++ result
    SubUnknown s -> "Unknown error: " ++ s

debugAdvent :: Integer -> Part -> (Text -> String) -> IO String
debugAdvent rawDay _ func = do
  _ <- loadFile defaultConfig
  sessionKey <- getEnv "ADVENT_SESSION_KEY"
  let opts = AoCOpts sessionKey 2020 (Just "./cache") False 3000
  input <- runAoC opts (AoCInput day)
  let result = func (fromRight (error "Oh no! Couldn't get the input") input)
  return result
  where
    day = unsafeMkDay rawDay

unsafeMkDay :: Integer -> Day
unsafeMkDay = fromMaybe (error "Couldn't parse that day") . mkDay
