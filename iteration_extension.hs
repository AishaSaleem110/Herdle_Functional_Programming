import System.IO
import Data.Time
import Base
import Data.Char

{-
This function assign symbol to each Status value to 
be displayed to guide user during the game
-}
interpStatus:: Status -> String
interpStatus Here = "Y"
interpStatus Nowhere = "-"
interpStatus (Elsewhere Nothing) = "y"
interpStatus (Elsewhere (Just Before)) = "<"
interpStatus (Elsewhere (Just After)) = ">"

{-
This function combines symbols for all Status values and returns a string representing 
the overall guidance of the characters of the word entered by the user 
- (x:xs) - represents the array containing Status for each charactered entered by user for the Guess
-}
showStatus:: [Status] -> String
showStatus [] = ""
showStatus (x:xs) = interpStatus x ++ " " ++ showStatus xs
  
{-
This function checks the guess of the user against the word of the day and
 returns a character wise status 
 - input - is the input guess by the user
 - word - is the actual word of the day
-}
checkGuess  :: String -> String -> [(Char, Status)]
checkGuess input word = checkGuessAux [] input word word 0

{-
This function is a helper auxilliary function for the checkGuess function as it is done using tail recursion
-acc- represents accumulator
-(x:xs)- represents guess input by user as a string of characters
-(y:ys)- represents actual word as a string of characters
-word-  represents a copy of actual word to be kept as a whole to check if any character exists in the word at any other index
-itr-  an integer variable to keep track of the current index of the string being iterated 
-}
checkGuessAux :: [(Char, Status)] -> String -> String -> String -> Int-> [(Char, Status)]
checkGuessAux acc [] _ _ _ = acc
checkGuessAux acc _ [] _ _= acc
checkGuessAux acc (x:xs) (y:ys) word itr | x==y = checkGuessAux (acc ++ [(x,Here)]) xs ys word $ itr+1
                                         | x `member` word &&  getGuessDirection x itr word == Just Before  = checkGuessAux (acc ++ [(x,Elsewhere (Just Before))]) xs ys word $ itr+1
                                         | x `member` word && getGuessDirection x itr word == Just After = checkGuessAux (acc ++ [(x,Elsewhere (Just After))]) xs ys word $itr+1
                                         | otherwise = checkGuessAux (acc ++ [(x,Nowhere)]) xs ys word $ itr+1

{-
- This function assigns a final direction to a character,
 if it exists in the actual word before current index, it assigns Before, 
 if it exists both before and after of the index being checked then direction is Before
 if it only exists after the index being checked then it assigns After
 if none of the above case it assigns Nothing

--inputs--
g_index - current index being checked
char - character of the guessed word to be checked in actual word
word - actual word of the day
-}

getGuessDirection:: Char->Int->String-> Maybe Direction
getGuessDirection char g_index  word =  do
    let directions = computeCharacterDirections char g_index word 
    let before = filter ( == Before) directions 
    let after =  filter (== After) directions
   
    if ( not (null before) &&  not (null after) ) || not (null before)
        then return Before
    else if length after > 0 
        then return After
    else
        Nothing

{-
- A helper function to compute directions for a character
 It returns a direction for each occurance of a character but it is not the final direction        
-}

computeCharacterDirections:: Char -> Int -> String -> [Direction]
computeCharacterDirections char g_index  word = do
    let char_in_word_indices = getCharIndices char word     
    getDirections char_in_word_indices g_index 

{-
- A helper function to iterate the array of indices representing the positions where guessed 
 character is found in original word and then assigns direction to each occurance    
-}

getDirections:: [Int]-> Int -> [Direction]
getDirections [] _  = []
getDirections (index:indices) g_index  =
    assignDirection g_index index : getDirections indices g_index 


-- A helper function which compares two indices and assigns a direction 'Before' or 'After' based on comparison
assignDirection :: Int -> Int -> Direction
assignDirection guess_index word_index | guess_index > word_index = Before
                                        | otherwise = After


--A function which returns list all occurences of a character in a word in an integer list
getCharIndices ::Char -> String -> [Int]
getCharIndices char word =  let
    zipped = zip word [0..length word]  in
      let (chars,indices) = unzip $ filter (\(a,b)-> char==a ) zipped in indices


-- Function to get the guess of user

getGuess :: Integer -> [Char]-> IO String
getGuess = getGuessAux "" 

getGuessAux :: [Char]->Integer -> [Char]-> IO String
getGuessAux acc 0 allowed_chars  = do
    putStrLn ""
    return acc
getGuessAux acc allowed_tries allowed_chars= do
   
    inp <-  getChar'
    let inp' = toLower inp
    -- if user enters '.' then it allows user to re-enter the last character
    if inp' == '.' && allowed_tries /=5  then
     
        (do putStr "\b \b" >> putStr "\b \b" >> putStr "\b \b"
            getGuessAux (init acc) (allowed_tries+1) allowed_chars)                  
        
    -- checks if user has entered the character which is allowed
    else if inp' `member` allowed_chars then
        (do
            putChar ' '
            let temp = getGuessAux (acc ++ [inp'] ) (allowed_tries - 1) allowed_chars in temp           
        )
    -- if user enters character which is not allowed; it doesn't let user enter it
    else
        (do putStr "\b \b"
            getGuessAux acc allowed_tries allowed_chars)

-- helper function to separate array of Status from a tuple of (Char, Status)
getUserInputStatus :: [(Char,Status)] -> [Status]
getUserInputStatus [] = []
getUserInputStatus ((a,b):xs) = b:getUserInputStatus xs


{-
- A function that loops to get different attempts of guess word from user and 
 check if guessed word matches the actual word
 - word - actual word of the day
 - allowed_chars - An array of charcters user is allowed to enter
 - n- Number of attempts user is allowed to do
 - attempt - Current attempt of user to prompt it        
-}
loop :: String -> [Char] -> Int -> Int-> IO ()

-- case when user has exhausted maximum allowed attempts
loop word allowed_chars 0 attempt= do
    putStrLn $ prompt Lose
    putStr "The word was: " >> putStrLn word

-- case when user still has attempts to guess the word    
loop word allowed_chars n attempt = do
    putStrLn $ "Attempt " ++ show attempt 
    putStr $ prompt Start ++ " "
    choice <- getChar'
    putStrLn ""
    case choice of
        'q'-> putStrLn $ prompt Quit
        _ -> do
        putStr $ prompt Guess
        inp <- getGuess 5 allowed_chars
        let char_status = checkGuess inp word
    
        let statuses = getUserInputStatus char_status
        putStrLn $ leftMargin ++ showStatus statuses

        --check if user has entered all correct characters
        let len = length $ filter (== Here) statuses
        case len of
            5 -> putStrLn $ prompt Win
            _ ->let updated_chars = updateAvailable allowed_chars char_status in
                 loop word updated_chars (n-1) $ attempt+1


-- Function that starts the game by calling the loop with allowed characters and 6 maximum attempts allowed
go :: String -> IO ()
go inp = do
     loop (map toLower inp) "abcdefghijklmnopqrstuvwxyz" 6 1
     return ()

-- Function to update the list of allowed/available characters to user after each attempt   
updateAvailable :: [Char] -> [(Char, Status)] -> [Char]
updateAvailable [] _ = []
updateAvailable x [] =  x
updateAvailable x guess = let
    (c,d) = unzip $ filter (\(a,b) -> b == Nowhere) guess in
        filter (\y-> not $ member y c) x

-- Function that checks if a character is a member of a given String and returns a Bool response
member :: Eq a=> a -> [a] -> Bool
member char = foldr (\a remaining -> char == a || remaining) False

--Helper function to create a String containing number of spaces as much as length of Start prompt
leftMargin :: String
leftMargin = leftMarginAux ""

leftMarginAux:: String -> String
leftMarginAux acc = do
    if length acc == length (prompt Start) then
     acc
    else
        leftMarginAux (acc ++ " ")

--Function that fetches word for the day from a file based on current day
getTodaysWord :: IO ()
getTodaysWord = do
    handle <- openFile "wordlist.txt" ReadMode
    contents <- hGetContents handle      
    let words = lines contents 
    now <- getZonedTime
    let (year,month,day) = toGregorian $ localDay $ zonedTimeToLocalTime now
    go $ words !! (day-1)
    hClose handle

-- main function
main :: IO ()
main = do   
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    getTodaysWord 
    putStrLn "Press Enter to exit. ">>getLine >>= putStrLn