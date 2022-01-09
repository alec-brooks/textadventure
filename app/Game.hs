module Game where

import Control.Monad (unless)
import System.IO
import Text.Show.Functions

import State (GameState, Command(Quit))
import ParseCommand (parseCommand)
import Evaluate (eval)
import FormatMessage (formatMessage)

-- todo:
--   help h
--   pretty print inventory
--   
--   look at room
--   put *Name in GameObjects
--   make Use safer. Add fail case text

read' = do
  putStr "> "
  hFlush stdout
  getLine

loop' :: GameState -> IO ()
loop' gs = do
  input <- read'
  let cmd = parseCommand input

  unless
    (cmd == Quit)
    ( do
        let newGameState = eval cmd gs
        putStrLn $ formatMessage newGameState
        loop' newGameState
    )
