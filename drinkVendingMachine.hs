---- DATA STRUCTURES ----

data Drink = Drink {name::String, cost::Int, quantity::Int} 
    deriving (Show, Read, Eq)

data Money = Money {m500::Int, m200::Int, m100::Int, m50::Int, m20::Int, m10::Int, m5::Int, m2::Int, m1::Int}
    deriving (Show, Read, Eq)

data Machine = Machine {password::String, state::[Drink], money::Money}
    deriving (Show, Read, Eq)

---- DATA STRUCTURES HELPERS ----

emptyWallet :: Money
emptyWallet = (Money 0 0 0 0 0 0 0 0 0)

connectMoney :: Money -> Money -> Money
connectMoney (Money m1 m2 m3 m4 m5 m6 m7 m8 m9) (Money n1 n2 n3 n4 n5 n6 n7 n8 n9) = (Money (m1+n1) (m2+n2) (m3+n3) (m4+n4) (m5+n5) (m6+n6) (m7+n7) (m8+n8) (m9+n9))

moneyToInt :: Money -> Int 
moneyToInt money = (m500 money * 500) + (m200 money * 200) + (m100 money * 100) + (m50 money * 50) + (m20 money * 20) + (m10 money * 10) + (m5 money * 5) + (m2 money * 2) + (m1 money * 1)

moneyToString :: Money -> String 
moneyToString money = "" ++
  (if m500 money > 0 then "5 PLN x " ++ (show $ m500 money) ++ ", " else "") ++
  (if m200 money > 0 then "2 PLN x " ++ (show $ m200 money) ++ ", " else "") ++
  (if m100 money > 0 then "1 PLN x " ++ (show $ m100 money) ++ ", " else "") ++
  (if m50 money > 0 then "0.5 PLN x " ++ (show $ m50 money) ++ ", " else "") ++
  (if m20 money > 0 then "0.2 PLN x " ++ (show $ m20 money) ++ ", " else "") ++
  (if m10 money > 0 then "0.1 PLN x " ++ (show $ m10 money) ++ ", " else "") ++
  (if m5 money > 0 then "0.05 PLN x " ++ (show $ m5 money) ++ ", " else "") ++
  (if m2 money > 0 then "0.02 PLN x " ++ (show $ m2 money) ++ ", " else "") ++
  (if m1 money > 0 then "0.01 PLN x " ++ (show $ m1 money) ++ ", " else "")

moneyToList :: Money -> [(Int, Int)]
moneyToList money = [(m500 money, 500), (m200 money, 200), (m100 money, 100), (m50 money, 50), (m20 money, 20), (m10 money, 10), (m5 money, 5), (m2 money, 2), (m1 money, 1)]

listToMoney :: [Int] -> Money
listToMoney [m500, m200, m100, m50, m20, m10, m5, m2, m1] = (Money m500 m200 m100 m50 m20 m10 m5 m2 m1)
listToMoney _ = (Money 0 0 0 0 0 0 0 0 0)

addDrink :: Machine -> Drink -> Machine 
addDrink (Machine pass drinks money) drink = (Machine pass (addDrink' drinks drink) money)   

addDrink' :: [Drink] -> Drink -> [Drink]
addDrink' [] drink = [drink]
addDrink' (current@(Drink name cost q):rest) new@(Drink nameN costN qN) = 
  if and [nameN == name, cost == costN]
  then (Drink name cost (q+qN)) : rest  
  else  current : (addDrink' rest new)

decrease :: Machine -> Int -> Machine
decrease (Machine pass drinks money) position = (Machine pass (decrease' drinks position) money)

decrease' :: [Drink] -> Int -> [Drink]
decrease' ((Drink name cost 1):rest) 1 = rest
decrease' ((Drink name cost q):rest) 1 = (Drink name cost (q-1)) : rest
decrease' (drink:rest) n = drink:(decrease' rest (n-1))

getCost :: Machine -> Int -> Int 
getCost (Machine pass drinks money) position = getCost' drinks position

getCost' :: [Drink] -> Int -> Int
getCost' ((Drink _ cost _):_) 1 = cost
getCost' (_:rest) n = getCost' rest (n-1)

getMaxID :: Machine -> Int 
getMaxID (Machine _ drinks _) = length drinks


---- TEST CASE ---

preparedCase :: IO ()
preparedCase = loop (Machine "12345" [
  (Drink "Bebsi" 200 10),
  (Drink "Kola" 210 12),
  (Drink "P FUEL" 330 5)] 
  (Money 1 2 3 4 8 2 5 2 2))

---- MAIN ----

main :: IO ()
main = loop (Machine "1234" [] (Money 0 0 0 0 0 0 0 0 0))

loop :: Machine -> IO ()
loop machine = do 
  putStr "Action: "
  action <- getLine
  
  case action of 
    "?" -> do 
              putStrLn "maintain machine -> m"
              putStrLn "buy drink -> b"
              putStrLn "exit -> x"
              loop machine
    "b" -> do 
              state <- buy machine
              loop state 
    "m" -> do 
              authorized <- checkPassword machine 3
              if authorized 
                then do 
                  newMachine <- maintain machine 
                  loop newMachine
                else do 
                  loop machine
    "x" -> do 
              return ()
    _ -> do 
              putStrLn "Unknown action"
              loop machine

---- USER PANEL ----

buy :: Machine -> IO (Machine) 
buy machine = do 
  putStr "Action: "
  action <- getLine
  case action of 
    "?" -> do 
              putStrLn "show list of available products -> l"
              putStrLn "chose drink -> c"
              putStrLn "go back -> b"
              buy machine
    "l" -> do 
              printListOfProducts machine
              buy machine
    "c" -> do 
              newMachine <- handleTransaction machine
              return newMachine
    "b" -> do 
              return machine
    _ -> do 
              putStrLn "Unknown action"
              buy machine

---- ADMIN PANEL ----

maintain :: Machine -> IO (Machine) 
maintain machine = do 
  putStr "Action: "
  action <- getLine
  case action of 
    "?" -> do 
              putStrLn "add drink -> d"
              maintain machine
    "d" -> do
              newMachine <- handleAddDrink machine
              return newMachine
    _ -> do 
              putStrLn "Unknown action"
              maintain machine

-- ADMIN PANEL HELPERS --

checkPassword :: Machine -> Int -> IO (Bool)
checkPassword _ 0 = do return (False) 
checkPassword machine@(Machine pass _ _) leftTimes = do 
  putStr "Password: "
  userPass <- getLine
  if userPass == pass 
    then do
      return (True)
    else do
      result <- checkPassword machine (leftTimes-1)
      return result


handleAddDrink :: Machine -> IO (Machine)
handleAddDrink machine = do 
  putStr "Drink name: "
  name <- getLine 
  putStr "Cost (grosze): "
  cost <- getCorrectNumber
  putStr "Count: "
  count <- getCorrectNumber
  let newMachine = addDrink machine (Drink name cost count)
  return (newMachine) 

-- USER PANEL HELPERS --

printListOfProducts :: Machine -> IO ()
printListOfProducts (Machine _ drinks _) = do
  printListOfProducts' drinks 1
  return () 
  
printListOfProducts' :: [Drink] -> Int -> IO ()
printListOfProducts' [drink] i = do
  printDrink drink i
  return ()
printListOfProducts' (drink:rest) i = do
  printDrink drink i
  printListOfProducts' rest (i+1)
  return ()

printDrink :: Drink -> Int -> IO ()
printDrink (Drink name cost _) i = do 
  putStrLn ( (show i) ++ ". " ++ name ++ ", cost: " ++ (show $ cost `div` 100) ++ "." ++ (if rest < 10 then "0" else "") ++ (show rest) )
  where
    rest = cost `mod` 100

---- TRANSACTION ----

handleTransaction :: Machine -> IO (Machine)
handleTransaction machine@(Machine pass state money) = do 
  productCode <- inputDrinkID (getMaxID machine) -- just use length ??? 
  let cost = getCost machine productCode
  userMoney <- getMoneyFromUser emptyWallet cost
  let changeValue = moneyToInt userMoney - cost
  if canGiveChange money userMoney changeValue
    then do
      let moneyStates = calculateWallets money userMoney changeValue 
      let machineMoney = fst moneyStates
      let change = snd moneyStates
      putStrLn "Successful transaction"
      if changeValue == 0 
        then putStrLn "No change"
        else putStrLn ("Your change: " ++ (moneyToString change))
      let updatedMachine = decrease (Machine pass state machineMoney) productCode
      return updatedMachine
    else do 
      putStrLn "Cannot give the change - transaction canceled"
      putStrLn ("Your money: " ++ (moneyToString userMoney))
      return machine


-- TRANSACTION HELPERS --

inputDrinkID :: Int -> IO (Int)
inputDrinkID max = do 
  putStr "Enter the number: "
  number <- getLine
  if isInteger number && (read number) <= max
    then do return (read number)
    else do
      putStrLn "Invalid code"
      result <- inputDrinkID max
      return result 

getMoneyFromUser :: Money -> Int -> IO (Money)
getMoneyFromUser all@(Money m500 m200 m100 m50 m20 m10 m5 m2 m1) limit = do
  if moneyToInt all >= limit 
    then return (all) 
    else do
          putStr "Insert coin: "
          coin <- getLine
          case coin of 
            "0.01" -> do 
              newMoney <- getMoneyFromUser (Money m500 m200 m100 m50 m20 m10 m5 m2 (m1+1)) limit
              return newMoney
            "0.02" -> do 
              newMoney <- getMoneyFromUser (Money m500 m200 m100 m50 m20 m10 m5 (m2+1) m1) limit
              return newMoney
            "0.05" -> do 
              newMoney <- getMoneyFromUser (Money m500 m200 m100 m50 m20 m10 (m5+1) m2 m1) limit
              return newMoney
            "0.1" -> do 
              newMoney <- getMoneyFromUser (Money m500 m200 m100 m50 m20 (m10+1) m5 m2 m1) limit
              return newMoney
            "0.2" -> do 
              newMoney <- getMoneyFromUser (Money m500 m200 m100 m50 (m20+1) m10 m5 m2 m1) limit
              return newMoney
            "0.5" -> do 
              newMoney <- getMoneyFromUser (Money m500 m200 m100 (m50+1) m20 m10 m5 m2 m1) limit
              return newMoney
            "1" -> do 
              newMoney <- getMoneyFromUser (Money m500 m200 (m100+1) m50 m20 m10 m5 m2 m1) limit
              return newMoney
            "2" -> do 
              newMoney <- getMoneyFromUser (Money m500 (m200+1) m100 m50 m20 m10 m5 m2 m1) limit
              return newMoney
            "5" -> do 
              newMoney <- getMoneyFromUser (Money (m500+1) m200 m100 m50 m20 m10 m5 m2 m1) limit
              return newMoney
            _ -> do 
              putStrLn "There is no such denomination"
              newMoney <- getMoneyFromUser all limit
              return newMoney

canGiveChange :: Money -> Money -> Int -> Bool
canGiveChange m1 m2 val = lastValue >= 0
  where lastValue = last $ snd $ calculateChange (moneyToList $ connectMoney m1 m2) val 

calculateChange :: [(Int,Int)] -> Int -> ([Int],[Int])
calculateChange [] 0 = ([],[])
calculateChange [] _ = ([-999999999999999],[-999999999999999])
calculateChange ((count,_):rest) 0 = (count:left, 0:right)
  where 
    subResult = calculateChange rest 0
    left = fst subResult
    right = snd subResult

calculateChange (state:rest) change = 
  if (value <= change) && (count /= 0)
    then (decLeft, (decValue+1):decRight)
    else (count : delLeft, 0 : delRight)
  where
    value = snd state 
    count = fst state
    decreaseSubResult = calculateChange ((count-1, value) : rest) (change - value)
    decLeft = fst decreaseSubResult
    (decValue:decRight) = snd decreaseSubResult
    delegateSubResult = calculateChange rest change
    delLeft = fst delegateSubResult
    delRight = snd delegateSubResult

calculateWallets :: Money -> Money -> Int -> (Money,Money) 
calculateWallets m1 m2 val = (listToMoney machine, listToMoney change)
  where
    (machine, change) = calculateChange (moneyToList $ connectMoney m1 m2) val


---- HELPERS ----

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

getCorrectNumber :: IO (Int)
getCorrectNumber = do 
  number <- getLine
  if isInteger number
    then do return (read number)
    else do
      putStrLn "Invalid number, try again: "
      result <- getCorrectNumber
      return result 