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

instance Show Stacks where
  show (Stacks a b c sel) =
    "A: " ++ show a ++ " B: " ++ show b ++ " C: " ++ show c ++ " sel: " ++ show sel

main :: IO ()
main = do
  print empty
  args <- getArgs
  case args of
    (filename:_) -> do
        contents <- readFile filename
        putStrLn contents
    [] -> putStrLn "Usage: runghc amnesia.hs <program.amn>"

