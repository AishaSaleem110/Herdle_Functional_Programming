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
  

checkGuess  :: String -> String -> [(Char, Status)]
checkGuess input word = checkGuessAux [] input word word 0


checkGuessAux :: [(Char, Status)] -> String -> String -> String -> Int-> [(Char, Status)]
checkGuessAux acc [] _ _ _ = acc
checkGuessAux acc _ [] _ _= acc
checkGuessAux acc (x:xs) (y:ys) word itr | x==y = checkGuessAux (acc ++ [(x,Here)]) xs ys word $ itr+1
                                         | x `member` word &&  isBefore x itr ( getCharHereIndices x acc) word  = checkGuessAux (acc ++ [(x,Elsewhere (Just Before))]) xs ys word $ itr+1
                                         | x `member` word  = checkGuessAux (acc ++ [(x,Elsewhere (Just After))]) xs ys word $itr+1
                                         | otherwise = checkGuessAux (acc ++ [(x,Nowhere)]) xs ys word $ itr+1

getCharHereIndices:: Char -> [(Char, Status)]-> [Int]
getCharHereIndices char statuses = 
    let (element, index) = unzip $ filter (\((a,b),y)->a== char && b == Here ) $ zip statuses [0..] 
    in index

isBefore:: Char->Int->[Int]->String-> Bool
isBefore char g_index here_indices word =  do
    let directions = computeCharacterDirections char g_index here_indices word
    let before = filter ( == Before) directions
    let after =  filter (== After) directions
    (length before > 0 && length after > 0)
     || (length before > 0) 


computeCharacterDirections:: Char -> Int -> [Int]-> String -> [Direction]
computeCharacterDirections char g_index here_indices word = do
    let char_in_word_indices = getCharIndices char word 
    let filtered_indices = filter (`notElem` here_indices) char_in_word_indices 
    getDirections filtered_indices g_index 



getDirections:: [Int]-> Int -> [Direction]
getDirections [] _  = []
getDirections (index:indices) g_index  =
    assignDirection g_index index : getDirections indices g_index 

assignDirection :: Int -> Int -> Direction
assignDirection guess_index word_index | guess_index > word_index = Before
                                        | otherwise = After

getCharIndices ::Char -> String -> [Int]
getCharIndices char word =  let
    zipped = zip word [0..length word]  in
      let (chars,indices) = unzip $ filter (\(a,b)-> char==a ) zipped in indices


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
    (c,d) = unzip $ filter (\(a,b) -> b == Nowhere) guess in
        filterChars x c

member :: Eq a=> a -> [a] -> Bool
member x = foldr (\y rest -> x == y || rest) False


leftMargin :: String
leftMargin = leftMarginAux ""

leftMarginAux:: String -> String
leftMarginAux acc = do
    if length acc == length (prompt Start) then
     acc
    else
        leftMarginAux (acc ++ "")

