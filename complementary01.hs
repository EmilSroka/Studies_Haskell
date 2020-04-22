-- Zadanie 1
-- Napisać program, który odwraca zadaną listę
reverse' [] = []
reverse' (head:tail) = reverse tail ++ [head]

-- Zadanie 2
-- Napisać program, który sprawdzi czy lista jest palindromem
isPalindrome list = list == reverse' list 

-- Zadanie 3
-- Napisać program, usuwający z listy duplikaty następujące bezpośrednio po sobie
deleteDuplicates [] = []
deleteDuplicates [single] = [single]
deleteDuplicates (first:rest) = if first == head(deleteDuplicates rest) 
                                  then deleteDuplicates rest
                                  else first : deleteDuplicates rest
