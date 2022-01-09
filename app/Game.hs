module Game where

import Control.Monad (unless)
import System.IO
import Text.Show.Functions

import State (GameState, Command(Quit))
import ParseCommand (parseCommand)
import Evaluate (eval)
import FormatMessage (formatMessage)

-- todo:
--   look at room
--   Add use fail case text

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
