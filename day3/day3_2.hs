import Data.Set
import qualified Data.List
import Data.Char (ord, isLower)
import Data.List.Split

getCommonLetter one two three = head (toList (intersection (intersection (fromList one) (fromList two)) (fromList three)))
scoreLetter letter = if (isLower letter) then (ord letter - 96) else (ord letter - 38)

scoreGroups [] = 0
scoreGroups (group:groups) = (scoreLetter (getCommonLetter (group!!0) (group!!1) (group!!2))) + (scoreGroups groups)

main = do
    content <- readFile "input.txt"
    let groups = chunksOf 3 (lines content)
    print (scoreGroups groups)