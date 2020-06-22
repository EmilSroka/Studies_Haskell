-- Zadanie
-- Utworzyć listę studentów:
module Student where

data Student = Student {firstName::String, lastName::String, age::Int} 
   deriving (Show, Read, Eq)

data StudentsFirstNameChangeEvent = StudentsFirstNameChangeEvent {oldName::String, newName::String}
   deriving (Show, Read)

listToProcess = [Student "Alicja" "Akla" 21, Student "Batrek" "Bodo" 20, Student "Celina" "Czyzyk" 21, Student "Damian" "Dab"  22, Student "Eustachy" "Elo" 20]
modifiedList = [Student "AlicjaX" "Akla" 21, Student "BatrekX" "Bodo" 20, Student "Celina" "Czyzyk" 21, Student "DamianX" "Dab"  22, Student "Eustachy" "Elo" 20]

-- Utworzyć listę zawierającą pełne imiona i nazwiska studentów w postaci łańcuchów znaków.   
fullNameList :: [Student] -> [String]
fullNameList [] = []
fullNameList ((Student firstName lastName _):rest) = (:) (firstName ++ " " ++ lastName) $ fullNameList rest

-- Utworzyć listę zawierającą pary w postaci krotek: numer porządkowy, student.
numberedList :: [Student] -> [(Int, Student)]
numberedList students = zip [1..] students 

-- Przetworzyć listę z powyższego punktu na raport tekstowy w formacie
-- 1. student: Nazwisko I. wiek: Wiek
-- 2. student: Nazwisko2 I2. wiek: Wiek2
createReport :: [Student] -> String
createReport x = processData $ numberedList x

processData :: [(Int, Student)] -> String
processData [] = ""
processData ((no,(Student firstName lastName age)):rest) = 
  (show no) ++ ". student: " ++ lastName ++ " " ++ [(head firstName)] ++ ". wiek: " ++ (show age) ++ "\n"
  ++ (processData rest)

-- Wygenerować tabelkę HTML.
createHtmlReport :: [Student] -> String
createHtmlReport x = 
  "<html><body><table>" ++
    "<tr>" ++ 
      "<th>" ++ "No." ++ "</th>" ++ 
      "<th>" ++ "First Name" ++ "</th>" ++ 
      "<th>" ++ "Last Name" ++ "</th>" ++ 
      "<th>" ++ "Age" ++ "</th>" ++ 
    "</tr>" ++ (processHtmlData $ numberedList x) ++
  "</table></body></html>"

processHtmlData :: [(Int, Student)] -> String 
processHtmlData [] = ""
processHtmlData ((no,(Student firstName lastName age)):rest) = 
  "<tr>" ++ 
    "<td>" ++ (show no) ++ "</td>" ++ 
    "<td>" ++ firstName ++ "</td>" ++ 
    "<td>" ++ lastName ++ "</td>" ++ 
    "<td>" ++ (show age) ++ "</td>" ++ 
  "</tr>"
  ++ (processHtmlData rest)

-- Wygenerować listę zmian w postaci typu wydarzenia, StudentsFirstNameChangeEvent oldName newName, przez utworzenie zmodyfikowanej listy studentów, a następnie porównanie (pod tymi samymi pozycjami powinni się znajdować ci sami studenci):
listOfChanges :: [Student] -> [Student] -> [StudentsFirstNameChangeEvent]
listOfChanges [] [] = []
listOfChanges ((Student oldName _ _):rest1) ((Student newName _ _):rest2) = 
  (if oldName /= newName then [(StudentsFirstNameChangeEvent oldName newName)] else [] )  
  ++ (listOfChanges rest1 rest2)

-- Dodać możliwość odczytu danych z pliku (punkt 1) i zapisu do pliku (punkty 3, 4).
readListFromFile :: String -> IO [Student]
readListFromFile fileName = do 
  contents <- readFile fileName
  return (map lineToStudent (split '\n' contents))

writeListToFile :: String -> [Student] -> IO ()
writeListToFile filename students = do   
  let result = foldl (\acc line -> acc ++ line ++ "\n") "\n" (map studentToLine students)
  writeFile filename result

writeReport :: String -> [Student] -> IO ()
writeReport filename students = do   
  let result = createReport students
  writeFile filename result

writeHtmlReport :: String -> [Student] -> IO ()
writeHtmlReport filename students = do   
  let result = createHtmlReport students
  writeFile filename result

-- heplers
studentToLine :: Student -> String
studentToLine (Student firstName lastName age) = "Student " ++ firstName ++ " " ++ lastName ++ " " ++ (show age)

lineToStudent :: String -> Student
lineToStudent line = (Student firstName lastName age)
  where 
    parts = split ' ' line
    firstName = parts!!1
    lastName = parts!!2
    age = read (parts!!3)

split :: Char -> String -> [String]
split _ "" = []
split delimiter [x] = if x == delimiter then [] else [[x]]
split delimiter (h:t) = if h == delimiter
  then []:(split delimiter t)
  else ((:) h $ head splitted):(tail splitted)
    where 
      splitted = split delimiter t