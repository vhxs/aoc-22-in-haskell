import Data.List.Split
import qualified Data.Set

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

moveRopeHeadOne cmd rope = (moveHeadOne cmd (head rope)) : (tail rope)
updateKnot rope knot newLoc = (take knot rope) ++ [newLoc] ++ (drop (knot+1) rope)

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

moveRopeN cmd n rope = if (n >= 10) then rope else if (n == 0) then (moveRopeN cmd 1 (moveRopeHeadOne cmd rope)) else (moveRopeN cmd (n+1) newRope)
    where headLoc = rope!!(n-1);
            tailLoc = rope!!n;
            newTail = maybeMoveTail tailLoc headLoc;
            newRope = updateKnot rope n newTail;

executeCmd cmd rope tailHistory = (newRope, newTailHistory)
    where newRope = moveRopeN cmd 0 rope;
            newTailHistory = tailHistory ++ [last newRope];

executeCmdN cmd 0 rope tailHistory = (rope, tailHistory)
executeCmdN cmd n rope tailHistory = executeCmdN cmd (n-1) newRope newTailHistory
    where newData = executeCmd cmd rope tailHistory;
            newRope = fst newData;
            newTailHistory = snd newData;

executeLine line rope tailHistory = executeCmdN cmd n rope tailHistory
    where cmd = head (splitOn " " line);
            n = read (last (splitOn " " line)) :: Int;

executeProgram [] rope tailHistory = (rope, tailHistory)
executeProgram (line:lines) rope tailHistory = executeProgram lines newRope newTailHistory
    where newData = executeLine line rope tailHistory;
            newRope = fst newData;
            newTailHistory = snd newData;

main = do
    content <- readFile "input.txt"
    let startingRope = take 10 (repeat (0, 0))
    let tailHistory = snd (executeProgram (lines content) startingRope [(0, 0)])
    print (length (Data.Set.fromList (tailHistory)))