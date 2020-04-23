-- Zadanie 1
-- a) zdefiniuj odpowiedni typ dla drzewa binarnego,
data Node a = Empty | Node a (Node a) (Node a) deriving (Show) 
type Tree a = Node a

-- b) zdefiniuj następujące funkcje:
-- insert - wstawienie elementu
insert :: Ord a => Tree a -> a -> Tree a
insert (Empty) el = Node el Empty Empty
insert (Node value left right) el 
    | value == el = Node value left right
    | value < el = Node value left (insert right el) 
    | value > el = Node value (insert left el) right

-- empty - sprawdzenie, czy drzewo jest puste
empty :: Tree a -> Bool
empty (Empty) = True
empty (Node _ _ _) = False

-- isBinary - sprawdzenie, czy drzewo jest drzewem BST
isBinary :: Ord a => Tree a -> Bool        
isBinary tree = traverseLVR tree == (quiksort $ traverseLRV tree)

-- search - sprawdzenie, czy element występuje w drzewie
search :: Eq a => Tree a -> a -> Bool 
search (Empty) _ = False
search (Node value left right) target = if target == value then True
                                        else (search left target) || (search right target)

-- isBalanced - sprawdzenie, czy drzewo jest zrównoważone
isBalanced :: Tree a -> Bool
isBalanced (Empty) = True 
isBalanced (Node _ left right) = (abs $ (height left) - (height right)) < 2 && isBalanced left && isBalanced right

-- traverse{VLR, LVR, LRV, VRL, RVL, RLV} - przejście po elementach drzewa na wszystkie możliwe sposoby
traverseVLR :: Tree a -> [a]
traverseVLR (Empty) = []
traverseVLR (Node value left right) = [value] ++ (traverseVLR left) ++ (traverseVLR right)

traverseLVR :: Tree a -> [a]
traverseLVR (Empty) = []
traverseLVR (Node value left right) = (traverseLVR left) ++ [value] ++ (traverseLVR right)

traverseLRV :: Tree a -> [a]
traverseLRV (Empty) = []
traverseLRV (Node value left right) = (traverseLRV left) ++ (traverseLRV right) ++ [value]

traverseVRL :: Tree a -> [a]
traverseVRL (Empty) = []
traverseVRL (Node value left right) = [value] ++ (traverseVRL right) ++ (traverseVRL left)

traverseRVL :: Tree a -> [a]
traverseRVL (Empty) = []
traverseRVL (Node value left right) = (traverseRVL right) ++ [value] ++ (traverseRVL left)

traverseRLV :: Tree a -> [a]
traverseRLV (Empty) = []
traverseRLV (Node value left right) = (traverseRLV right) ++ (traverseRLV left) ++ [value] 

-- toString - wypisanie drzewa w postaci „a(b(d,e),c(,f(g,)))”
toString :: Show a => Tree a -> [Char]
toString (Empty) = ""
toString (Node value Empty Empty) = show value
toString (Node value left right) = (show value) ++ "(" ++ (toString left) ++ "," ++ (toString right) ++ ")" 

-- leaves - zwracanie listy liści
leaves :: Tree a -> [a]
leaves (Empty) = []
leaves (Node value Empty Empty) = [value]
leaves (Node value left right) = (leaves left) ++ (leaves right)

-- nnodes - zwracanie liczby węzłów
nnodes :: Tree a -> Int
nnodes (Empty) = 0
nnodes (Node value left right) = 1 + (nnodes left) + (nnodes right)

-- nsum - zliczanie sumy wartości w węzłach
nsum :: Num a => Tree a -> a 
nsum (Empty) = 0
nsum (Node value left right) = value + (nsum left) + (nsum right)

-- tmap - odpowiednik funkcji map dla drzewa
tmap :: Tree a -> (a -> a) -> Tree a
tmap (Empty) _ = Empty
tmap (Node value left right) cb = Node (cb value) (tmap left cb) (tmap right cb)   

-- remove - usuwanie elementu
remove :: Ord a => Tree a -> a -> Tree a
remove (Empty) _ = Empty
remove (Node value Empty Empty) target = if value == target
                                         then Empty
                                         else Node value Empty Empty
remove (Node value left Empty) target = if value == target 
                                        then left
                                        else Node value (remove left target) Empty
remove (Node value Empty right) target = if value == target 
                                         then right 
                                         else Node value Empty (remove right target) 
remove (Node value left right) target = if value == target
                                        then Node (tmin right) left (remove right (tmin right))
                                        else Node value (remove left target) (remove right target)

-- merge - łączenie dwóch drzew
merge :: Num a => Tree a -> Tree a -> Tree a 
merge (Empty) (Empty) = Empty 
merge tree (Empty) = tree 
merge (Empty) tree = tree 
merge (Node v1 left1 right1) (Node v2 left2 right2) = Node (v1 + v2) (merge left1 left2) (merge right1 right2)  

-- helpers
height :: Tree a -> Int 
height (Empty) = 0
height (Node _ left right) = (max (height left) (height right)) + 1

tmin :: Ord a => Tree a -> a
tmin (Empty) = undefined
tmin (Node value Empty Empty) = value
tmin (Node value left Empty) = min value (tmin left)
tmin (Node value Empty right) = min value (tmin right)
tmin (Node value left right) = min (min (tmin left) (tmin right)) value

quiksort :: Ord a => [a] -> [a]
quiksort [] = []
quiksort (head:rest) = (quiksort lesser) ++ [head] ++ (quiksort greater) where 
                       lesser = filter (< head) rest 
                       greater = filter (>= head) rest