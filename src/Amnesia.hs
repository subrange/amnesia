module Main where

import System.Environment (getArgs)
import Stacks
import Parser

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

