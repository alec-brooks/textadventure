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
        assertEqual [] (GoTo Kitchen) (parseCommand "go and there are words in the middled as well, many of them kitchen")
    ]
