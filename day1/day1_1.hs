import Data.List.Split
import System.TimeIt

group_numbers lines = splitWhen (== "") lines

sum_list [] = 0
sum_list (elt:elts) = (read elt :: Integer) + sum_list elts

sum_all_lists [] = []
sum_all_lists (one_list:other_lists) = sum_list one_list : sum_all_lists other_lists

main = do
    content <- readFile "input.txt"
    timeIt $ print (maximum (sum_all_lists (group_numbers (lines content))))