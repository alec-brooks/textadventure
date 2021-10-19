module Main where
import System.IO
import Control.Monad (unless)

main :: IO ()
main = do
    input <- read'

    unless (input == ":quit")
        $ print' (eval' input) >> main

read' :: IO String
read' = putStr "REPL> "
    >> hFlush stdout
    >> getLine

eval' :: String -> String
eval' input = input

print' :: String -> IO ()
print' = putStrLn


