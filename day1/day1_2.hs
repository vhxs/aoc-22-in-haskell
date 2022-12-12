import Data.List.Split
import Data.List

group_numbers lines = splitWhen (== "") lines

reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

sum_list [] = 0
sum_list (elt:elts) = (read elt :: Integer) + sum_list elts

sum_all_lists [] = []
sum_all_lists (one_list:other_lists) = sum_list one_list : sum_all_lists other_lists

sum_top_three int_list = sum (take 3 (reverseList (sort int_list)))

main = do
    content <- readFile "input.txt"
    print (sum_top_three (sum_all_lists (group_numbers (lines content))))