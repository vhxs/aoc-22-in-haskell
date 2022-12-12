import Data.List.Split

initStacks = ["FTCLRPGQ", "NQHWRFSJ", "FBHWPMQ", "VSTDF", "QLDWVFZ", "ZCLS", "ZBMVDF", "TJB", "QNBGLSPH"]

amount line = read (splitOn "from" ((splitOn "move" line)!!1)!!0) :: Int
src line = read (splitOn "to" ((splitOn "from" line)!!1)!!0) :: Int
dst line = read (splitOn "to" ((splitOn "from" line)!!1)!!1) :: Int

takeLast n xs = drop (length xs - n) xs
reverseList xs = foldl (\x y -> y:x) [] xs 

takeTop src amount stacks = takeLast amount (stacks!!(src - 1)) 
dropTop src amount stacks = (take (src - 1) stacks) ++ [take (length (stacks!!(src - 1)) - amount) (stacks!!(src - 1))] ++ (drop src stacks)
putTop dst toPut stacks = (take (dst - 1) stacks) ++ [(stacks!!(dst - 1) ++ toPut)] ++ (drop dst stacks)

rearrange startingStacks [] = startingStacks
rearrange startingStacks (line:lines) = rearrange (putTop (dst line) (takeTop (src line) (amount line) startingStacks) (dropTop (src line) (amount line) startingStacks)) lines

main = do
    content <- readFile "input.txt"
    print (rearrange initStacks (drop 10 (lines content)))