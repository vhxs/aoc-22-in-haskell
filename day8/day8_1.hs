visibleFromLeft idx val row = all (< val) (take idx row)
visibleFromRight idx val row = all (< val) (drop (idx+1) row)

getRow idx lines = map (\x -> read x :: Int) (map (:[]) (lines!!idx))
getCol idx lines = map (\x -> read ((map (:[]) x)!!idx) :: Int) lines
getVal x y lines = (getRow x lines)!!y

visible coord lines = ((visibleFromLeft y val row) || (visibleFromRight y val row)) || ((visibleFromLeft x val col) || (visibleFromRight x val col))
    where x = fst coord;
            y = snd coord;
            val = getVal x y lines;
            row = getRow x lines;
            col = getCol y lines;

coords m n = [ (x,y) | x<-[0..(m-1)], y<-[0..(n-1)] ]

countVisible lines = sum (map (\x -> if (visible x lines) then 1 else 0) (coords (length lines) (length (lines!!0))))

main = do
    content <- readFile "input.txt"
    let ll = (lines content)
    print (countVisible ll)