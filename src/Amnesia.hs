module Main where

import System.Environment (getArgs)
import Stacks

data Instr
  = ChoA      -- select stack A
  | ChoB      -- select stack B
  | ChoC      -- select stack C
  | Set Int   -- set selected stack to specific value
  | Add       -- A = A + B
  | Sub       -- A = A - B
  | Mul       -- A = A * B
  | Div       -- A = A / B
  | Mod       -- A = A % B
  | Nm        -- non-zero check
  | Disp      -- print top of stack
  | Open      -- ":" open bracket
  | Close     -- ";" close bracket
  deriving(Show, Eq)

main :: IO ()
main = do
  let a = empty
  let b = push 10 a
  let c = empty
  let d = push 5 c
  let (v, k) = pop b
  print v
  print k
  print (peek d)
  --args <- getArgs
  --case args of
  --  (filename:_) -> do
  --      contents <- readFile filename
  --      putStrLn contents
  --  [] -> putStrLn "Usage: runghc amnesia.hs <program.amn>"

