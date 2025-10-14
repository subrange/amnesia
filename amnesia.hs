import System.Environment (getArgs)

data S = A | B | C
  deriving(Show, Eq)

data Stacks = Stacks
  { a   :: [Int]
  , b   :: [Int]
  , c   :: [Int]
  , sel :: S
  }

empty :: Stacks
empty = Stacks [] [] [] A

push :: Int -> Stacks -> Stacks
push x sts = case sel sts of
  A -> sts { a = x : a sts }
  B -> sts { b = x : b sts }
  C -> sts { c = x : c sts }

instance Show Stacks where
  show (Stacks a b c sel) =
    "A: " ++ show a ++ " B: " ++ show b ++ " C: " ++ show c ++ " sel: " ++ show sel

main :: IO ()
main = do
  let a = empty
  let b = push 10 a
  print b
  --args <- getArgs
  --case args of
  --  (filename:_) -> do
  --      contents <- readFile filename
  --      putStrLn contents
  --  [] -> putStrLn "Usage: runghc amnesia.hs <program.amn>"

