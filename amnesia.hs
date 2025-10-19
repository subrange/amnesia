import System.Environment (getArgs)

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

peek :: Stacks -> Maybe Int
peek sts = case sel sts of
  A -> peekAt (a sts)
  B -> peekAt (b sts)
  C -> peekAt (c sts)
  where
    peekAt []   = Nothing -- stack empty
    peekAt(x:_) = Just x  -- get top value

instance Show Stacks where
  show (Stacks a b c sel) =
    "A: " ++ show a ++ " B: " ++ show b ++ " C: " ++ show c ++ " sel: " ++ show sel

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

