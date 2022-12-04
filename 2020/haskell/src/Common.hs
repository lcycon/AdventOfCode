{-# LANGUAGE OverloadedStrings #-}

module Common
  ( module Common,
    module Text.Parsec.Text,
    module Data.Text,
  )
where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Read (decimal)
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)

type Impl = Text -> String

readLines :: (Text -> a) -> Text -> [a]
readLines parse = fmap parse . Text.lines

asInt :: Text -> Int
asInt input = case decimal input of
  Left s -> error $ "Error reading integer input: " ++ s
  Right (r, "") -> r
  Right (_, rest) -> error $ "Failed reading integer input, leftover: " ++ show rest

intP :: Parser Int
intP = read <$> P.many1 P.digit

charP :: Char -> Parser Char
charP = P.char

anyCharP :: Parser Char
anyCharP = P.anyChar

whitespaceP :: Parser ()
whitespaceP = void $ P.many (P.choice [P.space, P.tab])

withWhitespaceP :: Parser a -> Parser a
withWhitespaceP p = whitespaceP *> p <* whitespaceP

equalP :: Parser Char
equalP = P.char '='

alphaP :: Parser String
alphaP = P.many1 P.letter

readParse :: Parser a -> Text -> a
readParse parser = either handleError id . P.parse (parser <* P.eof) ""
  where
    handleError e = error $ "Failed to parse: " ++ show e

readLinesParse :: Parser a -> Text -> [a]
readLinesParse parser = readParse (P.sepEndBy parser P.endOfLine)
