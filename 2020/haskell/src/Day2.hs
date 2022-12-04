module Day2 (part1, part2) where

import Common
  ( Impl,
    Parser,
    alphaP,
    anyCharP,
    charP,
    intP,
    readLinesParse,
    withWhitespaceP,
  )

data PasswordEntry = PasswordEntry Int Int Char String
  deriving (Eq, Show)

part1 :: Impl
part1 = show . length . filter isValidPassword . readInput
  where
    isValidPassword (PasswordEntry low high c str) = isBetween low high (countLetters c str)
    readInput = readLinesParse passwordEntryP
    countLetters chr = length . filter (== chr)
    isBetween low high v = v >= low && v <= high

part2 :: Impl
part2 = show . length . filter isValidPassword . readInput
  where
    isValidPassword (PasswordEntry a b c str) = (charAt a str == c) /= (charAt b str == c)
    charAt idx = (!! (idx - 1)) -- For the challenge, indices are 1-based
    readInput = readLinesParse passwordEntryP

passwordEntryP :: Parser PasswordEntry
passwordEntryP = PasswordEntry <$> (intP <* charP '-') <*> intP <*> withWhitespaceP (anyCharP <* charP ':') <*> alphaP
