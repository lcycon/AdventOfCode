module Day4 where

import           Prelude                 hiding ( max
                                                , min
                                                )

import           Data.List                      ( group
                                                , tails
                                                , transpose
                                                , unfoldr
                                                )
import           Data.Text                      ( Text )

min, max :: Int
min = 357253
max = 892942

minDigits, maxDigits :: [Int]
minDigits = intToDigitList min
maxDigits = intToDigitList max

problem1 :: Text -> IO ()
problem1 _ = print (length part1Candidates)

problem2 :: Text -> IO ()
problem2 _ = print (length part2Candidates)

part1Candidates :: [[Int]]
part1Candidates = filter (\c -> isInRange c && containsDouble c) candidates
  where
    isInRange x = x >= minDigits && x <= maxDigits
    containsDouble :: [Int] -> Bool
    containsDouble = any pairEqual . window 2
      where
        pairEqual [a, b] = a == b
        pairEqual _      = False
    candidates :: [[Int]]
    candidates =
        [ [a, b, c, d, e, f]
        | a <- [0 .. 9]
        , b <- [a .. 9]
        , c <- [b .. 9]
        , d <- [c .. 9]
        , e <- [d .. 9]
        , f <- [e .. 9]
        ]

part2Candidates :: [[Int]]
part2Candidates = filter (hasRunOf 2) part1Candidates
    where hasRunOf x = any ((== x) . length) . group

intToDigitList :: Int -> [Int]
intToDigitList = reverse . unfoldr go
  where
    go v | v >= 10   = Just (v `mod` 10, v `div` 10)
         | v >= 0    = Just (v, -1)
         | otherwise = Nothing

window :: Int -> [a] -> [[a]]
window count = filter ((== count) . length) . transpose . take count . tails
