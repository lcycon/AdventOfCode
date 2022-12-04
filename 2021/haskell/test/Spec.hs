{-# LANGUAGE OverloadedStrings #-}

import Advent (Part (Part1))
import Data.Text (Text)
import qualified Day1
import Lib (debugAdvent)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

checkPart :: Integer -> Part -> (Text -> String) -> String -> SpecWith ()
checkPart day part func answer = describe ("Day" ++ show day) $ do
  it ("passes " ++ show part) $ do
    result <- debugAdvent day part func
    result `shouldBe` answer

main :: IO ()
main = hspec $ do
  checkPart 1 Part1 Day1.part1 "539851"
  -- checkPart 1 Part2 Day1.part2 "212481360"
