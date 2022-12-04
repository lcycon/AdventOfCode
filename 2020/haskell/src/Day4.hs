{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Day4 (part1, part2) where

import qualified Barbies as B
import Common (Impl, Parser, alphaP, charP, readParse)
import Control.Monad ((<=<))
import Data.Char (isDigit, isHexDigit, isSpace)
import Data.Functor.Identity (Identity (Identity))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic, M1 (M1))
import Refined (FromTo, Refined, SizeEqualTo, refineFail)
import Text.Parsec
  ( char,
    endOfLine,
    many1,
    notFollowedBy,
    satisfy,
    sepBy,
    sepEndBy1,
    try,
    (<?>),
    (<|>),
  )
import Text.Read (readMaybe)

type RawRecord = HM.HashMap String String

data Height
  = HeightCM (Refined (FromTo 150 193) Int)
  | HeightIN (Refined (FromTo 59 76) Int)
  deriving (Eq, Show)

data EyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth
  deriving (Eq, Show)

eyeColorFromString :: String -> Maybe EyeColor
eyeColorFromString "amb" = Just Amb
eyeColorFromString "blu" = Just Blu
eyeColorFromString "brn" = Just Brn
eyeColorFromString "gry" = Just Gry
eyeColorFromString "grn" = Just Grn
eyeColorFromString "hzl" = Just Hzl
eyeColorFromString "oth" = Just Oth
eyeColorFromString _ = Nothing

data Record f = Record
  { byr :: f (Refined (FromTo 1920 2002) Int),
    iyr :: f (Refined (FromTo 2010 2020) Int),
    eyr :: f (Refined (FromTo 2020 2030) Int),
    hgt :: f Height,
    hcl :: f (Refined (SizeEqualTo 6) String),
    ecl :: f EyeColor,
    pid :: f (Refined (SizeEqualTo 9) [Refined (FromTo 0 9) Int])
  }
  deriving (Generic, B.FunctorB, B.TraversableB, B.ConstraintsB)

deriving instance B.AllBF Show f Record => Show (Record f)

newtype RecordReader a = RecordReader {unRecordReader :: RawRecord -> Maybe a}

recordReader :: Record RecordReader
recordReader =
  Record
    { byr = fromField "byr" (refineFail <=< readMaybe),
      iyr = fromField "iyr" (refineFail <=< readMaybe),
      eyr = fromField "eyr" (refineFail <=< readMaybe),
      hgt = fromField "hgt" refineHgt,
      hcl = fromField "hcl" refineHcl,
      ecl = fromField "ecl" eyeColorFromString,
      pid = fromField "pid" refinePid
    }
  where
    fromField k f = RecordReader (f <=< HM.lookup k)
    refineHgt input = case suffix of
      "cm" -> fmap HeightCM . refineFail <=< readMaybe $ num
      "in" -> fmap HeightIN . refineFail <=< readMaybe $ num
      _ -> Nothing
      where
        (num, suffix) = span isDigit input
    refineHcl ('#' : rest)
      | all isHexDigit rest = refineFail rest
      | otherwise = Nothing
    refineHcl _ = Nothing
    refinePid = refineFail <=< traverse (refineFail <=< readMaybe . pure)

readRecord :: RawRecord -> Maybe (Record Identity)
readRecord rr = B.btraverse (fmap Identity . ($ rr) . unRecordReader) recordReader

requiredFields :: HS.HashSet String
requiredFields = HS.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

recordP :: Parser RawRecord
recordP = HM.fromList <$> sepEndBy1 entryP (char ' ' <|> delimP)
  where
    delimP = try $ endOfLine <* notFollowedBy endOfLine
    entryP = ((,) <$> alphaP <*> (charP ':' >> many1 (satisfy (not . isSpace)))) <?> "Entry"

inputP :: Parser [RawRecord]
inputP = sepBy recordP (endOfLine >> endOfLine)

part1 :: Impl
part1 input = show . length . filter (HS.null . HS.difference requiredFields . HM.keysSet) $ records
  where
    records = readParse inputP input

part2 :: Impl
part2 input = show . length . mapMaybe readRecord $ records
  where
    records = readParse inputP input
