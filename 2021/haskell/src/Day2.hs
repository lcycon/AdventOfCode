module Day2 (part1, part2) where

import Common (Impl, parseLines, tokenParser)
import Data.Foldable (Foldable (foldl'))
import Text.Parsec.Char (spaces, string)
import Text.Parsec.Combinator (choice)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser (decimal))

data Command
  = Forward Int
  | Down Int
  | Up Int

commandP :: Parser Command
commandP = choice [forward, down, up]
  where
    commandBaseP m c = do
      _ <- string m
      spaces
      num <- decimal tokenParser
      return . c . fromInteger $ num
    forward = commandBaseP "forward" Forward
    down = commandBaseP "down" Down
    up = commandBaseP "up" Up

part1 :: Impl
part1 = show . uncurry (*) . foldl' reduce (0, 0) . parseLines commandP
  where
    reduce (h, v) (Forward d) = (h + d, v)
    reduce (h, v) (Up d) = (h, v - d)
    reduce (h, v) (Down d) = (h, v + d)

part2 :: Impl
part2 = show . (\(_, h, v) -> h * v) . foldl' reduce (0, 0, 0) . parseLines commandP
  where
    reduce (aim, h, v) (Forward d) = (aim, h + d, v + (aim * d))
    reduce (aim, h, v) (Up d) = (aim - d, h, v)
    reduce (aim, h, v) (Down d) = (aim + d, h, v)
