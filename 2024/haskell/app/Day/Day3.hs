module Day.Day3 where

import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.ParserCombinators.ReadP (ReadP, (<++))
import qualified Text.ParserCombinators.ReadP as R
import Util (digitBoundedIntP)

intP :: ReadP Int
intP = digitBoundedIntP 1 3

data Mul = Mul Int Int
  deriving (Show)

evalMul :: Mul -> Int
evalMul (Mul x y) = x * y

mulP :: ReadP Mul
mulP = Mul <$> (R.string "mul(" *> intP) <*> (R.char ',' *> intP <* R.char ')')

data Instruction = MulI Mul | DoI | DontI
  deriving (Show)

instructionP :: ReadP Instruction
instructionP = (MulI <$> mulP) <++ (DoI <$ doP) <++ (DontI <$ dontP)
 where
  doP = R.string "do()"
  dontP = R.string "don't()"

manyMulP :: ReadP [Mul]
manyMulP = catMaybes <$> R.many1 ((Just <$> mulP) <++ (R.get $> Nothing))

manyInstructionP :: ReadP [Instruction]
manyInstructionP = catMaybes <$> R.many1 ((Just <$> instructionP) <++ (R.get $> Nothing))

part1 :: Text -> IO Int
part1 input = case R.readP_to_S (manyMulP <* R.eof) (Text.unpack input) of
  (results, _) : _ -> pure . sum . map evalMul $ results
  _ -> error "Bad parse"

data ProgramState = ProgramState {mulEnabled :: Bool, result :: Int}

defaultProgramState :: ProgramState
defaultProgramState = ProgramState True 0

part2 :: Text -> IO Int
part2 input = case R.readP_to_S (manyInstructionP <* R.eof) (Text.unpack input) of
  (results, _) : _ -> pure . (.result) $ runProgram results
  _ -> error "Bad parse"
 where
  go acc DoI = acc{mulEnabled = True}
  go acc DontI = acc{mulEnabled = False}
  go acc@ProgramState{mulEnabled = True} (MulI (Mul x y)) = acc{result = acc.result + (x * y)}
  go acc _ = acc
  runProgram = foldl go defaultProgramState
