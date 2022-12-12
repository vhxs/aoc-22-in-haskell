getRow idx lines = map (\x -> read x :: Int) (map (:[]) (lines!!idx))
getCol idx lines = map (\x -> read ((map (:[]) x)!!idx) :: Int) lines

reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

countLess [elt] val = 1
countLess (elt:elts) val = if elt < val then 1 + (countLess elts val) else 1

countFromLeft idx row = countLess (reverseList (take idx row)) (row!!idx)
countFromRight idx row = countLess (drop (idx+1) row) (row!!idx)

score coord lines = (countFromLeft y row) * (countFromRight y row) * (countFromLeft x col) * (countFromRight x col)
    where x = fst coord;
            y = snd coord;
            row = getRow x lines;
            col = getCol y lines;

coords m n = [ (x,y) | x<-[1..(m-2)], y<-[1..(n-2)] ]

maxScore lines = maximum (map (\x -> score x lines) (coords (length lines) (length (lines!!0))))

main = do
    content <- readFile "input.txt"
    let ll = (lines content)
    print (maxScore ll)