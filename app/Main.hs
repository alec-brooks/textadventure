module Main where
import Game (loop', startingGS)

main :: IO ()
main = do
  putStrLn "Yo"
  loop' startingGS
