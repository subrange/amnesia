module Parser
  ( Instr(..)
  -- , parseProgram
  , cho -- fixme: for testing
  ) where

import Stacks (S(..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

data Instr
  = Cho S  -- choose stack A/B/C
  | Set    -- =
  | Add    -- +
  | Sub    -- -
  | Mul    -- *
  | Div    -- /
  | Mod    -- %
  | Disp   -- disp
  | Nm     -- conditional check
  | Open   -- ':'
  | Close  -- ';'
  deriving(Show, Eq)

type Parser = Parsec Void String

cho :: Parser Instr
cho = do
  _ <- string "cho"
  s <- satisfy (`elem` "ABC")
  return $ case s of
    'A' -> Cho A
    'B' -> Cho B
    'C' -> Cho C
    _  -> error "?"
