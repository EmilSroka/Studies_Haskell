-- Zadanie 1
-- Napisz następujące funkcje używając fold{r,l}:
-- sum
sum' :: (Num a) => [a] -> a
sum' numbers = foldl (\accumulator value -> accumulator + value) 0 numbers
-- product
product' :: (Num a) => [a] -> a
product' numbers = foldl (\accumulator value -> accumulator * value) 1 numbers
-- reverse
reverse' :: [a] -> [a]
reverse' array = foldl (\accumulator value -> value : accumulator) [] array

reverse'' :: [a] -> [a]
reverse'' array = foldr (\value accumulator -> accumulator ++ [value]) [] array
-- and
and' :: [Bool] -> Bool
and' logicalValues = foldl (\accumulator value -> accumulator && value) True logicalValues
-- or
or' :: [Bool] -> Bool
or' logicalValues = foldl (\accumulator value -> accumulator || value) False logicalValues
-- head
head' :: [a] -> a
head' array = foldr (\value _ -> value) undefined array
-- last
last' :: [a] -> a
last' array = foldl (\_ value  -> value) undefined array