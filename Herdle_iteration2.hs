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

-- checkGuess :: String -> String -> [(Char, Status)]
-- checkGuess [] _ = []
-- checkGuess _ [] = [] 
-- checkGuess (x:xs) (y:ys) | x == y = (x,Here): checkGuess xs ys
--                          | otherwise = (x,Nowhere): checkGuess xs ys   

checkGuess  :: String -> String -> [(Char, Status)] 
checkGuess x y = checkGuessAux [] x y y

checkGuessAux :: [(Char, Status)] -> String -> String -> String ->[(Char, Status)]
checkGuessAux acc [] _ _ = acc
checkGuessAux acc _ [] _ = acc 
checkGuessAux acc (x:xs) (y:ys) aux | x==y = checkGuessAux (acc ++ [(x,Here)]) xs ys aux
                                    | x `member` aux = checkGuessAux (acc ++ [(x,Elsewhere Nothing)]) xs ys aux
                                    | otherwise = checkGuessAux (acc ++ [(x,Nowhere)]) xs ys aux

getGuess :: Integer -> [Char]-> IO String
getGuess 0 allowed_chars  = do
    putStrLn ""
    return []
getGuess n allowed_chars= do
    inp <-  getChar'
    let inp' = toLower inp
    if inp' `member` allowed_chars then

        (do putChar ' '
            xs <- getGuess (n - 1) allowed_chars
            return (inp' : xs))
    else
        (do putStr "\b \b"
            getGuess n allowed_chars)  

--helper function
getUserInputStatus :: [(Char,Status)] -> [Status]
getUserInputStatus [] = []
getUserInputStatus ((a,b):xs) = b:getUserInputStatus xs


loop :: String -> [Char] -> Int -> IO ()    
loop _ allowed_chars 0 = do
    putStrLn $ prompt Lose
loop word allowed_chars n = do
    putStrLn "Enter a guess of five letter word: " 
    --putStrLn allowed_chars
    inp <- getGuess 5 allowed_chars
    let char_status = checkGuess inp word
    --putStrLn $ show char_status
    let statuses = getUserInputStatus char_status
    putStrLn $ showStatus statuses 
    let len = length $ filter (== Here) statuses
    case len of
        5 -> putStrLn $ prompt Win
        _ ->let updated_chars = updateAvailable allowed_chars char_status in
                 loop word updated_chars (n-1)
    

go :: String -> IO ()
go inp = do
     loop (map toLower inp) "abcdefghijklmnopqrstuvwxyz" 6
     return ()

--helper function
filterChars :: [Char] -> String -> [Char]
filterChars [] _ = []
filterChars x [] = x
filterChars (x:xs) s| x `member`s  = filterChars xs s
                              | otherwise = x:filterChars xs s 

updateAvailable :: [Char] -> [(Char, Status)] -> [Char]
updateAvailable [] _ = []
updateAvailable x [] =  x
updateAvailable x guess = let
    (c,d) = unzip $ filter (\(a,b) -> (b == Nowhere)) guess in 
        filterChars x c 
                              

member :: Eq a=> a -> [a] -> Bool
member x = foldr (\y rest -> (x == y) || rest) False