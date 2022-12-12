import Data.List.Split

getFirstInterval line = map (\x -> read x :: Integer) (splitOn "-" (head (splitOn "," line)))
getSecondInterval line = map (\x -> read x :: Integer) (splitOn "-" (last (splitOn "," line)))

overlap one two = (((head one) <= (last two)) && ((last one) >= (head two))) || (((head two) <= (last one)) && ((last two) >= (head one)))

countOverlap [] = 0
countOverlap (line:lines) = (if (overlap (getFirstInterval line) (getSecondInterval line)) then 1 else 0) + countOverlap lines

main = do
    content <- readFile "input.txt"
    print (countOverlap (lines content))