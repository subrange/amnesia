import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filename:_) -> do
        contents <- readFile filename
        putStrLn contents
    [] -> putStrLn "Usage: runghc amnesia.hs <program.amn>"

