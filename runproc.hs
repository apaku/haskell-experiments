import System.Process

main = do
    line <- getLine
    output <- readProcess "echo" ["You typed: ",line] []
    putStrLn output
