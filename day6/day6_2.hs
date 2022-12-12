import qualified Data.Set

getStartOfPacket idx line = if (length (Data.Set.toList (Data.Set.fromList (take 14 line))) == 14) then (idx + 14) else getStartOfPacket (idx + 1) (tail line)

main = do
    content <- readFile "input.txt"
    print (getStartOfPacket 0 (head (lines content)))