{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Common
  ( module Common,
    module Data.Text,
  )
where

import Advent (Day, Part (..), mkDay)
import Data.Functor.Identity (Identity)
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Data.Text.Read (decimal)
import Text.Parsec (parse)
import qualified Text.Parsec.Char as PC
import qualified Text.Parsec.Combinator as PCO
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as PTok

type Impl = Text -> String

readLines :: (Text -> a) -> Text -> [a]
readLines parseF = fmap parseF . Text.lines

parseInput :: Parser a -> Text -> a
parseInput p = handleError . parse p "input" . unpack
  where
    handleError = either buildError id
    buildError err = error $ "Failed to parse: " ++ show err

parseLines :: Parser a -> Text -> [a]
parseLines p = handleError . parse fullParser "input" . unpack
  where
    fullParser = PCO.sepEndBy1 p PC.newline
    handleError = either buildError id
    buildError err = error $ "Failed to parse: " ++ show err

asInt :: Text -> Int
asInt input = case decimal input of
  Left s -> error $ "Error reading integer input: " ++ s
  Right (r, "") -> r
  Right (_, rest) -> error $ "Failed reading integer input, leftover: " ++ show rest

parseDayPart :: String -> Maybe (Day, Part)
parseDayPart = either (const Nothing) Just . parse inputP ""
  where
    inputP :: Parser (Day, Part)
    inputP = (,) <$> (PC.char 'd' *> dayP) <*> (PC.char 'p' *> partP)
    dayP :: Parser Day
    dayP = maybe (fail "Cannot parse day") return . mkDay =<< PTok.decimal tokenParser
    partP :: Parser Part
    partP =
      PTok.decimal tokenParser >>= \case
        1 -> return Part1
        2 -> return Part2
        _ -> fail "Cannot parse part"

tokenParser :: PTok.GenTokenParser String u Identity
tokenParser = PTok.makeTokenParser emptyDef
