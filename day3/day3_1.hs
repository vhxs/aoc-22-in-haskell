import Data.Set
import qualified Data.List
import Data.Char (ord, isLower)

takeLast n xs = Data.List.drop (length xs - n) xs

leftHalfSet line = fromList (Data.List.take ((length line) `div` 2) line)
rightHalfSet line = fromList (takeLast ((length line) `div` 2) line)

getCommonLetter line = head (toList (intersection (leftHalfSet line) (rightHalfSet line)))
scoreLetter letter = if (isLower letter) then (ord letter - 96) else (ord letter - 38)

scoreLines [] = 0
scoreLines (line:lines) = (scoreLetter (getCommonLetter line)) + (scoreLines lines)

main = do
    content <- readFile "input.txt"
    print (scoreLines (lines content))