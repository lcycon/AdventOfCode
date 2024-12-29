{-# LANGUAGE OverloadedStrings #-}

module Main where

import Advent (AoC (AoCInput, AoCSubmit), AoCUserAgent (AoCUserAgent), Part (Part1, Part2), SubmitRes (..), defaultAoCOpts, runAoC_)
import Advent.Types (mkDay_)
import Control.Monad (when)
import Data.Map (Map, fromList, (!))
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text)
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import Text.Printf (printf)
import Text.Read (readMaybe)

import qualified Day.Day1
import qualified Day.Day2
import qualified Day.Day3
import qualified Day.Day4

partMap :: Map (Int, Int) (Text -> IO Int)
partMap =
  fromList
    [ ((1, 1), Day.Day1.part1)
    , ((1, 2), Day.Day1.part2)
    , ((2, 1), Day.Day2.part1)
    , ((2, 2), Day.Day2.part2)
    , ((3, 1), Day.Day3.part1)
    , ((3, 2), Day.Day3.part2)
    , ((4, 1), Day.Day4.part1)
    , ((4, 2), Day.Day4.part2)
    ]

mkPart :: Int -> Part
mkPart 1 = Part1
mkPart 2 = Part2
mkPart _ = error "Invalid Part number"

printUsage :: IO ()
printUsage = putStrLn "Usage: aoc2024 <day> <part>"

checkUsage :: IO (Int, Int, Bool)
checkUsage = do
  args <- getArgs
  case args of
    [dayString, partString] -> do
      let day = readMaybe dayString
          part = readMaybe partString
      when (isNothing day) (putStrLn "Day should be a number")
      when (isNothing part) (putStrLn "Part should be a number")
      return (fromJust day, fromJust part, False)
    [dayString, partString, "submit"] -> do
      let day = readMaybe dayString
          part = readMaybe partString
      when (isNothing day) (putStrLn "Day should be a number")
      when (isNothing part) (putStrLn "Part should be a number")
      return (fromJust day, fromJust part, True)
    _ -> printUsage >> exitFailure

main :: IO ()
main = do
  (day, part, submit) <- checkUsage
  printf "Running Day %d Part %d\n" day part
  key <- getEnv "AOC_KEY"
  let aocOpts = defaultAoCOpts (AoCUserAgent "lcycon/AdventOfCode" "luke@lukecycon.com") 2024 key
  input <- runAoC_ aocOpts $ AoCInput (mkDay_ $ fromIntegral day)
  answer <- (partMap ! (day, part)) input
  printf "Answer: %d\n" answer
  when submit $ do
    putStrLn "Submitting..."
    (_, result) <- runAoC_ aocOpts $ AoCSubmit (mkDay_ (fromIntegral day)) (mkPart part) (show answer)
    case result of
      SubCorrect _ -> putStrLn "Correct!"
      SubIncorrect _ _ -> putStrLn "Incorrect :("
      SubWait sec -> printf "Rate limit, cool your jets! Just %d more seconds" sec
      _ -> putStrLn "Unknown error..."
    pure ()
