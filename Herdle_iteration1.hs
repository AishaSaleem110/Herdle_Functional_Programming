import Base
import Data.Char
--helper function
interpStatus:: Status -> String
interpStatus Here = "Y"
interpStatus Nowhere = "-"
interpStatus (Elsewhere Nothing) = "y"
interpStatus (Elsewhere (Just Before)) = "<"
interpStatus (Elsewhere (Just After)) = ">"

showStatus:: [Status] -> String
showStatus [] = ""
showStatus (x:xs) = interpStatus x ++ " " ++ showStatus xs 
--showStatus xs = foldr ((++) . interpStatus) "" xs

checkGuess :: String -> String -> [(Char, Status)]
checkGuess [] _ = []
checkGuess _ [] = [] 
checkGuess (x:xs) (y:ys) | x == y = (x,Here): checkGuess xs ys
                         | otherwise = (x,Nowhere): checkGuess xs ys   

getGuess :: Integer ->  IO String
getGuess 0  = do
    putStrLn ""
    return []
getGuess n = do
    inp <-  getChar'
    let inp' = toLower inp    
    putChar ' '    
    xs <- getGuess (n-1)
    return (inp':xs)    

--helper function
getUserInputStatus :: [(Char,Status)] -> [Status]
getUserInputStatus [] = []
getUserInputStatus ((a,b):xs) = b:getUserInputStatus xs


loop :: String -> Int -> IO ()    
loop _ 0 = do
    putStrLn $ prompt Lose
loop word n = do
    putStrLn "Enter a guess of five letter word"
    inp <- getGuess 5
    let statuses = getUserInputStatus ( checkGuess word inp)
    putStrLn $ showStatus statuses 
    let len = length $ filter (== Here) statuses
    case len of
        5 -> putStrLn $ prompt Win
        _ -> loop word (n-1)
    
go :: String -> IO ()
go inp = do
     loop (map toLower inp) 6
     return ()

    



        







