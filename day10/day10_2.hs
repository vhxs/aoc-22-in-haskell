import Data.List.Split

isNoop line = (head (splitOn " " line)) == "noop"
getX line = read ((splitOn " " line)!!1) :: Int
currentState history = (last history)

advanceState line history = if (isNoop line) then (history ++ [state]) else (history ++ [state, state + (getX line)]) where state = (currentState history)

executeProgram [] history = history
executeProgram (line:lines) history = executeProgram lines (advanceState line history)

modCycle cycle = mod cycle 40

getPixel cycle history = if (((modCycle cycle) >= position - 1) && ((modCycle cycle) <= position + 1)) then "#" else "." where position = history!!cycle
getPixels history = map (\x -> (getPixel x history)) [0..((length history) - 1)]

main = do
    content <- readFile "input.txt"
    let history = executeProgram (lines content) [1]
    print history
    print (getPixels history)