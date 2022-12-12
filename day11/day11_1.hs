graph = [[2, 3], [6, 5], [1, 7], [2, 7], [0, 3], [6, 4], [4, 0], [1, 5]]
startingItems = [[56, 52, 58, 96, 70, 75, 72],[75, 58, 86, 80, 55, 81],[73, 68, 73, 90],[72, 89, 55, 51, 59],[76, 76, 91],[88],[64, 63, 56, 50, 77, 55, 55, 86],[79, 58]]
divisibility = [11, 3, 5, 7, 19, 2, 13, 17]
operations = [\x -> x * 17, \x -> x + 7, \x -> x * x, \x -> x + 1, \x -> x * 3, \x -> x + 4, \x -> x + 8, \x -> x + 6]

processOneItem idx item = ((operations!!idx) item) `div` 3
itemGoesTo idx newItem = if (mod newItem (divisibility!!idx) == 0) then graph!!idx!!0 else graph!!idx!!1
removeItem idx initItems = (take idx initItems) ++ [tail (initItems!!idx)] ++ (drop (idx+1) initItems)
updateItems newItem newLoc initItems = (take newLoc initItems) ++ [(initItems!!newLoc) ++ [newItem]] ++ (drop (newLoc+1) initItems)
updateInspectCounts idx inspectCounts = (take idx inspectCounts) ++ [(inspectCounts!!idx) + 1] ++ (drop (idx+1) inspectCounts)

processItems idx initItems inspectCounts = if (null (initItems!!idx)) then (initItems, inspectCounts) else (processItems idx newItems newInspectCounts)
    where newItems = updateItems newItem newLoc removedItems;
            newItem = processOneItem idx (head (initItems!!idx));
            newLoc = itemGoesTo idx newItem;
            removedItems = removeItem idx initItems;
            newInspectCounts = updateInspectCounts idx inspectCounts;

processRound ptr initItems inspectCounts = if (ptr > 7) then (initItems, inspectCounts) else (processRound (ptr + 1) newItems newInspectCounts)
    where newReturn = (processItems ptr initItems inspectCounts);
            newItems = fst newReturn;
            newInspectCounts = snd newReturn;

processRounds count initItems inspectCounts = if (count >= 20) then (initItems, inspectCounts) else (processRounds (count + 1) newItems newInspectCounts)
    where newReturn = (processRound 0 initItems inspectCounts);
            newItems = fst newReturn;
            newInspectCounts = snd newReturn;

main = do
    print (processRounds 0 startingItems [0, 0, 0, 0, 0, 0, 0, 0])