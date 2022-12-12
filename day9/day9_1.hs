import Data.List.Split
import qualified Data.Set

getFirst (x, _, _) = x
getSecond (_, y, _) = y
getThird (_, _, z) = z

moveUp headLoc = (fst headLoc, snd headLoc + 1)
moveDown headLoc = (fst headLoc, snd headLoc - 1)
moveRight headLoc = (fst headLoc + 1, snd headLoc)
moveLeft headLoc = (fst headLoc - 1, snd headLoc)

moveHeadOne cmd headLoc
    | cmd == "U" = moveUp headLoc
    | cmd == "D" = moveDown headLoc
    | cmd == "L" = moveLeft headLoc
    | cmd == "R" = moveRight headLoc
    | otherwise = error "error"

l1Distance headLoc tailLoc = maximum [(abs (fst headLoc - fst tailLoc)), (abs (snd headLoc - snd tailLoc))]

getMove headLoc tailLoc
    | (fst headLoc > fst tailLoc) && (snd headLoc > snd tailLoc) = (1, 1)
    | (fst headLoc < fst tailLoc) && (snd headLoc > snd tailLoc) = (-1, 1)
    | (fst headLoc > fst tailLoc) && (snd headLoc < snd tailLoc) = (1, -1)
    | (fst headLoc < fst tailLoc) && (snd headLoc < snd tailLoc) = (-1, -1)
    | (fst headLoc == fst tailLoc) && (snd headLoc > snd tailLoc) = (0, 1)
    | (fst headLoc == fst tailLoc) && (snd headLoc < snd tailLoc) = (0, -1)
    | (fst headLoc > fst tailLoc) && (snd headLoc == snd tailLoc) = (1, 0)
    | (fst headLoc < fst tailLoc) && (snd headLoc == snd tailLoc) = (-1, 0)
    | otherwise = error "error"

moveTail tailLoc vector = (fst tailLoc + fst vector, snd tailLoc + snd vector)

maybeMoveTail tailLoc headLoc = if ((l1Distance headLoc tailLoc) <= 1) then tailLoc else moveTail tailLoc (getMove headLoc tailLoc)

executeCmd cmd headLoc tailLoc tailHistory = (newHead, newTail, newTailHistory)
    where newHead = moveHeadOne cmd headLoc;
            newTail = maybeMoveTail tailLoc newHead;
            newTailHistory = tailHistory ++ [newTail];

executeCmdN cmd 0 headLoc tailLoc tailHistory = (headLoc, tailLoc, tailHistory)
executeCmdN cmd n headLoc tailLoc tailHistory = executeCmdN cmd (n-1) newHead newTail newTailHistory
    where newData = executeCmd cmd headLoc tailLoc tailHistory;
            newHead = getFirst newData;
            newTail = getSecond newData;
            newTailHistory = getThird newData;

executeLine line headLoc tailLoc tailHistory = executeCmdN cmd n headLoc tailLoc tailHistory
    where cmd = head (splitOn " " line);
            n = read (last (splitOn " " line)) :: Int;

executeProgram [] headLoc tailLoc tailHistory = (headLoc, tailLoc, tailHistory)
executeProgram (line:lines) headLoc tailLoc tailHistory = executeProgram lines newHead newTail newTailHistory
    where newData = executeLine line headLoc tailLoc tailHistory;
            newHead = getFirst newData;
            newTail = getSecond newData;
            newTailHistory = getThird newData;

main = do
    content <- readFile "input.txt"
    let tailHistory = getThird (executeProgram (lines content) (0, 0) (0, 0) [(0, 0)])
    print (length (Data.Set.fromList (tailHistory)))