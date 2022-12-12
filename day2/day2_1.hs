import Data.List.Split

to_game line = splitOn " " line
to_games [] = []
to_games (line:lines) = to_game line : to_games lines

score one two = 
    if ((one == "A") && (two == "X")) then 1 + 3
    else if (one == "A") && (two == "Y") then 2 + 6
    else if (one == "A") && (two == "Z") then 3 + 0
    else if (one == "B") && (two == "X") then 1 + 0
    else if (one == "B") && (two == "Y") then 2 + 3
    else if (one == "B") && (two == "Z") then 3 + 6
    else if (one == "C") && (two == "X") then 1 + 6
    else if (one == "C") && (two == "Y") then 2 + 0
    else if (one == "C") && (two == "Z") then 3 + 3
    else 0

score_game game = score (head game) (last game)

score_games [] = 0
score_games (game:games) = (score_game game) + (score_games games)

main = do 
    content <- readFile "input.txt"
    print (score_games (to_games (lines content)))