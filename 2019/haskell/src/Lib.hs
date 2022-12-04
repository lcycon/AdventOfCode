{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( readCommaSepInts
    , readCommaSepSignedInts
    , readCommaSepSignedIntegers
    , readIntLines
    , readCustomLines
    )
where

import           Prelude                 hiding ( getContents )

import           Data.Text                      ( Text )

import qualified Data.Attoparsec.Text          as P

readCommaSepSignedIntegers :: Text -> [Integer]
readCommaSepSignedIntegers =
    readFileWith (P.signed P.decimal `P.sepBy'` P.char ',')

readCommaSepSignedInts :: Text -> [Int]
readCommaSepSignedInts =
    readFileWith (P.signed P.decimal `P.sepBy'` P.char ',')

readCommaSepInts :: Text -> [Int]
readCommaSepInts = readFileWith (P.decimal `P.sepBy'` P.char ',')

readCustomLines :: P.Parser a -> Text -> [a]
readCustomLines p = readFileWith (p `P.sepBy'` P.endOfLine)

readIntLines :: Text -> [Int]
readIntLines = readFileWith (P.decimal `P.sepBy'` P.endOfLine)

readFileWith :: P.Parser a -> Text -> a
readFileWith parser text = either
    (\f -> error $ "Could not consume input: " ++ f)
    id
    result
    where result = P.parseOnly (parser <* P.many' P.space <* P.endOfInput) text
