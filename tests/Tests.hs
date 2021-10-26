import Game (Command (..), LocationName (..), parseCommand)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main = defaultMain unitTests

unitTests =
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
      testCase "go command returns invalid for unknown locations" $
        assertEqual [] Invalid (parseCommand "go to bin")
    ]
