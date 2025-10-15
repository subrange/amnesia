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

pop :: Stacks -> (Maybe Int, Stacks)
pop sts = case sel sts of
  A -> popAt a (\xs -> sts { a = xs }) sts
  B -> popAt b (\xs -> sts { b = xs }) sts
  C -> popAt c (\xs -> sts { c = xs }) sts
  where
    popAt f update s = case f s of
      []     -> (Nothing, s) -- stack empty
      (x:xs) -> (Just x, update xs) -- pop head


instance Show Stacks where
  show (Stacks a b c sel) =
    "A: " ++ show a ++ " B: " ++ show b ++ " C: " ++ show c ++ " sel: " ++ show sel

main :: IO ()
main = do
  let a = empty
  let b = push 10 a
  let (v, c) = pop b
  print v
  print c
  --args <- getArgs
  --case args of
  --  (filename:_) -> do
  --      contents <- readFile filename
  --      putStrLn contents
  --  [] -> putStrLn "Usage: runghc amnesia.hs <program.amn>"

