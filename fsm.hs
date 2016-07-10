import Data.Char (toUpper, toLower)
import Data.Maybe (catMaybes)

data MachineState = Normal | Comment | Upper | Lower deriving (Show, Eq)

machine_cycle :: MachineState -> Char -> (Maybe Char, MachineState)
machine_cycle state c = case (state, c) of
  (Normal, '#') -> (Nothing, Comment)
  (Normal, '^') -> (Nothing, Upper)
  (Normal, '_') -> (Nothing, Lower)
  (Normal, c) -> (Just c, Normal)
  (Comment, '#') -> (Nothing, Normal)
  (Comment, c) -> (Nothing, Comment)
  (Upper, '^') -> (Nothing, Normal)
  (Upper, c) -> (Just (toUpper c), Upper)
  (Lower, '_') -> (Nothing, Normal)
  (Lower, c) -> (Just (toLower c), Lower)

run_machine :: MachineState -> String -> [Maybe Char] -> [Maybe Char]
run_machine _ [] result = result
run_machine state (x:xs) result = run_machine newState xs newResult
  where
    (maybeChar, newState) = machine_cycle state x
    newResult = result ++ [maybeChar]

runIt :: String -> String
runIt input = catMaybes $ run_machine Normal input []

testString = "42 65 27 _Moon_ ^small^ #fuck the man#"
