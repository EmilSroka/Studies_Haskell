import System.Random 

-- Napisać program kodujący daną listę zgodnie z algorytmem RLE
rle:: Eq a => [a] -> [(Int, a)]
rle list = map (\part -> (length part, head part)) (group list)

-- Napisać program generujący losową permutację elementów z listy
randomPerm:: [a] -> IO [a]
randomPerm list = do 
    let permutations = perms list
    let lastIndex = length permutations - 1
    gen <- getStdGen
    let random = head $ randomRs (0, lastIndex) gen
    return (permutations!!random)

-- Napisać program sortujący listy na podstawie długości jej wyrazów
sortByLength:: Ord a => [[a]] -> [[a]] 
sortByLength [] = []
sortByLength (head:rest) = (sortByLength shorter) ++ [head] ++ (sortByLength longerOrEqual) 
  where
    lengthOfHead = length head
    shorter = filter (\item -> length item < lengthOfHead) rest 
    longerOrEqual = filter (\item -> length item >= lengthOfHead) rest

-- helpers
group:: Eq a => [a] ->[[a]] 
group [] = []
group [a] = [[a]]
group (head:rest) = if previous == head 
                    then [head:previous:same] ++ others
                    else [head]:grouped
                  where
                    grouped@((previous:same):others) = group rest

perms:: [a] -> [[a]]
perms [] = [[]]
perms (head:rest) = perms rest >>= (insert' head)

insert':: x -> [x] -> [[x]]
insert' x [] = [[x]]
insert' new (head:rest) = [new:head:rest] ++ ( map (head:) (insert' new rest) )