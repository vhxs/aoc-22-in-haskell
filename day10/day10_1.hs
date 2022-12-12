import Data.List.Split

isNoop line = (head (splitOn " " line)) == "noop"
getX line = read ((splitOn " " line)!!1) :: Int
currentState history = (last history)

advanceState line history = if (isNoop line) then (history ++ [state]) else (history ++ [state, state + (getX line)]) where state = (currentState history)

executeProgram [] history = history
executeProgram (line:lines) history = executeProgram lines (advanceState line history)

getStrength cycle history = (history!!(cycle - 1)) * cycle

main = do
    content <- readFile "input.txt"
    let history = executeProgram (lines content) [1]
    print (sum [getStrength 20 history, getStrength 60 history, getStrength 100 history, getStrength 140 history, getStrength 180 history, getStrength 220 history])