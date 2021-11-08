import Data.Set (Set)
import qualified Data.Set as Set
import Game
  ( Command (..),
    GameState,
    LocationName (..),
    Object (..),
    ObjectName (..),
    book,
    command,
    currentLocation,
    eval',
    formatMessage,
    navigableLocations,
    parseCommand,
    startingGS,
  )
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Tests"
    [parserTests, evalTests, e2eTests]

parserTests =
  testGroup
    "Parser Tests"
    [ testCase "Exit becomes quit command" $
        assertEqual [] Quit (parseCommand "exit"),
      testCase "locations becomes location command" $
        assertEqual [] ViewLocations (parseCommand "locations"),
      testCase "go and location at start and end cause go action" $
        assertEqual [] (GoTo Kitchen) (parseCommand "go to kitchen"),
      testCase "just go and location at start and end cause go action" $
        assertEqual [] (GoTo Kitchen) (parseCommand "go kitchen"),
      testCase "go and location at start and end cause go action with words inside" $
        assertEqual [] (GoTo Kitchen) (parseCommand "go and there are words in the middled as well, many of them kitchen"),
      testCase "go command is case insensitive" $
        assertEqual [] (GoTo Kitchen) (parseCommand "go to Kitchen"),
      testCase "interact object works " $
        assertEqual [] (Interact Book) (parseCommand "pick up book")
    ]

evalTests =
  testGroup
    "Eval Tests"
    [ testCase "udpates command" $
        assertEqual [] Quit (command $ eval' Quit startingGS),
      testCase "current location is updated after move" $
        assertEqual [] Kitchen (currentLocation $ eval' (GoTo Kitchen) startingGS {navigableLocations = Set.singleton Kitchen})
    ]

printTests =
  testGroup
    "Print Tests"
    []

app input gs = do
  let cmd = parseCommand input
  formatMessage $ eval' cmd gs

e2eTests =
  testGroup
    "End to End Tests"
    [ testCase "examine items" $
        assertEqual [] (objDesc book) (app "examine book" startingGS),
      testCase "interact items" $
        assertEqual [] (interactText book ++ "\nKey added to inventory") (app "open book" startingGS),
      -- this should look nicer
      testCase "look at inventory" $
        assertEqual [] "fromList []" (app "inventory" startingGS)
    ]
