module Main where
import Game (loop')
import GameObjects (startingGS)

main :: IO ()
main = do
  putStrLn "Yo"
  loop' startingGS
