import Data.Map ((!))
import Data.Set (Set)
import qualified Data.Set as Set
import Evaluate (eval)
import FormatMessage (formatMessage, wrapText)
import GameObjects (book, startingGS)
import ParseCommand (parseCommand)
import State
  ( Command (..),
    GameState,
    ItemName (..),
    Language (..),
    LocationName (..),
    Object (..),
    ObjectName (..),
    command,
    currentLocation,
    inventory,
    navigableLocations,
  )
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
  testGroup
    "Tests"
    [parserTests, evalTests, e2eTests, formatTests]

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
        assertEqual [] (GoTo Kitchen) (parseCommand "go words in the middle kitchen"),
      testCase "go command is case insensitive" $
        assertEqual [] (GoTo Kitchen) (parseCommand "go to Kitchen"),
      testCase "interact object works " $
        assertEqual [] (Interact Book) (parseCommand "pick up book"),
      testCase "use item on object" $
        assertEqual [] (Use Key Door) (parseCommand "use key on door")
    ]

evalTests =
  testGroup
    "Eval Tests"
    [ testCase "udpates command" $
        assertEqual [] Quit (command $ eval Quit startingGS),
      testCase "current location is updated after move" $
        assertEqual [] Kitchen (currentLocation $ eval (GoTo Kitchen) kitchenAccessible)
    ]
  where
    kitchenAccessible = startingGS {navigableLocations = Set.singleton Kitchen}

printTests =
  testGroup
    "Print Tests"
    []

app input gs = do
  let cmd = parseCommand input
  formatMessage $ eval cmd gs

e2eTests =
  testGroup
    "End to End Tests"
    [ testCase "examine items" $
        assertEqual [] (objDesc book ! English) (app "examine book" startingGS),
      testCase "interact items" $
        assertEqual
          []
          (interactText book ! English ++ "\nKey added to inventory")
          (app "open book" startingGS),
      testCase "look at inventory " $
        assertEqual
          []
          "Inventory Contents:\nKey\nKnife"
          (app "inventory" startingGS {inventory = Set.fromList [Key, Knife]}),
      testCase "use bad item on object" $
        assertEqual
          []
          "That doesn't seem like a valid action"
          (app "use x on door" startingGS),
      testCase "use bad item on bad object" $
        assertEqual
          []
          "That doesn't seem like a valid action"
          (app "use x on x" startingGS),
      testCase "use item on bad object" $
        assertEqual
          []
          "That doesn't seem like a valid action"
          (app "use key on x" startingGS),
      testCase "use unowned item on object" $
        assertEqual
          []
          "That item is not in your inventory"
          (app "use key on door" startingGS)
    ]

testText = "hello there the wrap will be before the word wrap"

wrapped = "hello there the\nwrap will be before the\nword wrap"

bigText = "i heart verisimilitude in the morning"

bigWrapped = "i heart\nverisimilitude in the morning"

formatTests =
  testGroup
    "Formatter Tests"
    [ testCase "wrap line at index, before word" $
        assertEqual [] wrapped (wrapText 20 testText),
      testCase "big words" $
        assertEqual [] bigWrapped (wrapText 20 bigText)
    ]
