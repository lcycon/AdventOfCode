{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Day5 (part1, part2) where

import qualified Barbies as B
import Common (Impl, Text)
import Control.Monad ((<=<))
import Data.Functor.Identity (Identity (..))
import Data.List (sort, tails)
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import GHC.Generics (Generic, M1 (M1))
import Refined (Refined, SizeEqualTo, refineFail, unrefine)

data SeatCodeFBEntry = SeatFront | SeatBack

parseSeatCodeFBEntry :: Char -> Maybe SeatCodeFBEntry
parseSeatCodeFBEntry 'F' = Just SeatFront
parseSeatCodeFBEntry 'B' = Just SeatBack
parseSeatCodeFBEntry _ = Nothing

data SeatCodeLREntry = SeatLeft | SeatRight

parseSeatCodeLREntry :: Char -> Maybe SeatCodeLREntry
parseSeatCodeLREntry 'L' = Just SeatLeft
parseSeatCodeLREntry 'R' = Just SeatRight
parseSeatCodeLREntry _ = Nothing

data Seat' f = Seat
  { seatCodeFB :: f (Refined (SizeEqualTo 7) [SeatCodeFBEntry]),
    seatCodeLR :: f (Refined (SizeEqualTo 3) [SeatCodeLREntry])
  }
  deriving (Generic, B.FunctorB, B.TraversableB)

type Seat = Seat' Identity

getSeatRow :: Seat -> Int
getSeatRow = go 0 127 . unrefine . runIdentity . seatCodeFB
  where
    go low _ [] = low
    go low high (op : rest) = case op of
      SeatFront -> go low middle rest
      SeatBack -> go (middle + 1) high rest
      where
        middle = low + (high - low) `div` 2

getSeatColumn :: Seat -> Int
getSeatColumn = go 0 7 . unrefine . runIdentity . seatCodeLR
  where
    go low _ [] = low
    go low high (op : rest) = case op of
      SeatLeft -> go low middle rest
      SeatRight -> go (middle + 1) high rest
      where
        middle = low + (high - low) `div` 2

getSeatID :: Seat -> Int
getSeatID seat = getSeatRow seat * 8 + getSeatColumn seat

newtype SeatParser a = SeatParser {unSeatParser :: String -> Maybe a}

seatParser :: Seat' SeatParser
seatParser =
  Seat
    { seatCodeFB = SeatParser $ refineFail <=< traverse parseSeatCodeFBEntry . take 7,
      seatCodeLR = SeatParser $ refineFail <=< traverse parseSeatCodeLREntry . take 3 . drop 7
    }

readSeatCode :: Text -> Maybe Seat
readSeatCode input = B.btraverse (fmap Identity . ($ inputString) . unSeatParser) seatParser
  where
    inputString = Text.unpack input

readInput :: Text -> [Seat]
readInput = mapMaybe readSeatCode . Text.lines

part1 :: Impl
part1 = show . maximum . fmap getSeatID . readInput

windows :: Int -> [a] -> [[a]]
windows windowSize = foldr (zipWith (:)) (repeat []) . take windowSize . tails

part2 :: Impl
part2 = format . head . filter hasGap . windows 2 . sort . fmap getSeatID . readInput
  where
    format [a, _] = show $ a + 1
    format _ = error "Should never happen"
    hasGap [a, b] = b - a == 2
    hasGap _ = error "Should never happen"
