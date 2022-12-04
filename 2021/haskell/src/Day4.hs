{-# LANGUAGE TupleSections #-}

module Day4 (part1, part2, boardP, rowP, problemP) where

import Common
import qualified Data.HashMap.Strict as HM
import qualified Text.Parsec.Char as PChar
import qualified Text.Parsec.Combinator as PC
import Text.Parsec.String
import qualified Text.Parsec.Token as PT

data Problem = Problem { header :: [Int], boards :: [Board] }
  deriving (Eq, Show)

newtype Board = Board {entries :: HM.HashMap (Int, Int) (Int, Bool)}
  deriving (Eq, Show)

boardP :: Parser Board
boardP = Board . HM.fromList . mangle <$> PC.count 5 (rowP <* PC.optional PChar.spaces)
  where
    mangle xs = do
      (rowIdx, row) <- zip [0 ..] xs
      (colIdx, item) <- zip [0 ..] row
      return ((rowIdx, colIdx), item)

rowP :: Parser [(Int, Bool)]
rowP = PC.count 5 ((,False) . fromIntegral <$> entryP)
  where
    entryP = PC.optional PChar.spaces *> PT.decimal tokenParser

headerP :: Parser [Int]
headerP = PC.sepBy1 (fromIntegral <$> PT.decimal tokenParser) (PChar.char ',')

problemP :: Parser Problem
problemP = Problem <$> headerP <*> (PChar.space *> PC.sepBy1 boardP PChar.spaces)

part1 :: Impl
part1 = undefined

part2 :: Impl
part2 = undefined
