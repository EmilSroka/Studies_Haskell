-- Zadanie 1
-- Proszę wypisać listę zawierającą krotki reprezentujące poprawne trójkąty o bokach o długości x takich, że 3<x<17
triangles = [ 
             (x,y,z) | 
             x <- [4..16], y <- [4..16], z <- [4..16], 
             x < y + z, y < x + z, z < x + y,
             x <= y, y <= z
            ]
-- Proszę z tych trójkątów wybrać trójkąty prostokątne 
rightTriangle = [ 
             (x,y,z) | 
             x <- [4..16], y <- [4..16], z <- [4..16], 
             x < y + z, y < x + z, z < x + y,
             x <= y, y <= z,
             x^2 + y^2 == z^2
            ]

-- Zadanie 2
-- Proszę zdefiniować własne wersje funkcji:
-- * head,
head' (result:_) = result
-- * length,
length' [] = 0
length' (_:rest) = result where result = length' rest + 1
-- * take,
take' _ [] = []
take' 0 _ = []
take' n (head:rest) = head : take' (n-1) rest 
-- * map,
map' cb [] = []
map' cb (head:rest)= cb head : map' cb rest
-- * ++
(+++) [] list = list
(+++) (head:rest) list = head : rest +++ list