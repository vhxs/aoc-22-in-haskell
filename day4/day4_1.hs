import Data.List.Split

getFirstInterval line = map (\x -> read x :: Integer) (splitOn "-" (head (splitOn "," line)))
getSecondInterval line = map (\x -> read x :: Integer) (splitOn "-" (last (splitOn "," line)))

firstInSecond one two = ((head one) >= (head two)) && ((last one) <= (last two))
secondInFirst one two = ((head two) >= (head one)) && ((last two) <= (last one))

orderable one two = (firstInSecond one two) || (secondInFirst one two)

countOrderable [] = 0
countOrderable (line:lines) = (if (orderable (getFirstInterval line) (getSecondInterval line)) then 1 else 0) + countOrderable lines

main = do
    content <- readFile "input.txt"
    print (countOrderable (lines content))