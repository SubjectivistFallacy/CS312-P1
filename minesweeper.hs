import Data.List
import Data.List.Split      -- cabal install split --lib
import System.IO
import System.Random        -- cabal install random --lib
import Data.Char
import Data.Time.Clock
import Data.Time.LocalTime
import Control.Monad (when)
import System.Directory
import Control.Exception
import System.IO.Error

-- we modified this from https://github.com/EthanDY/cs312_project
data Grid = Grid {
    location :: (Int, Int), 
    mined :: Bool, 
    reached :: Bool,
    flagged :: Bool,
    num :: Int      -- Number of mines around the grid
} deriving (Show)

-- Record of player who clears the single play mode https://github.com/EthanDY/cs312_project
data Record = Record {
    difficulty :: String,
    name :: String,
    time :: Int
} deriving (Show)

-- https://github.com/EthanDY/cs312_project
instance Eq Record where
    (Record _ _ i1) == (Record _ _ i2) = i1 == i2

-- https://github.com/EthanDY/cs312_project
instance Ord Record where
    (Record _ _ i1) `compare` (Record _ _ i2) = i1 `compare` i2

-- First element of a tuple https://github.com/EthanDY/cs312_project
firTup (a,b) = a
-- Second element of a tuple https://github.com/EthanDY/cs312_project
secTup (a,b) = b

-- Return the negate of a bool https://github.com/EthanDY/cs312_project 
mynot True = False
mynot False = True

-- Build a row of the game board https://github.com/EthanDY/cs312_project
initRow :: Int -> Int -> Int -> [(Int, Int)] -> [Grid]
initRow x y length z
    | elem (x, y) z == True = Grid {location = (x, y), mined = True, reached = False, flagged = False, num = 0} : initRow x (y+1) length z
    | y < length = Grid {location = (x, y), mined = False, reached = False, flagged = False, num = 0} : initRow x (y+1) length z
    | otherwise = []

-- Build the game board https://github.com/EthanDY/cs312_project
buildBoard :: Int -> Int -> Int -> Int -> [(Int, Int)] -> [[Grid]]
buildBoard x y width length z
    | x < width = initRow x y length z: (buildBoard (x+1) y width length z)
    | otherwise = []

-- return a list of all possible combinations of two lists of elements https://github.com/EthanDY/cs312_project
allPairs :: [a] -> [b] -> [(a, b)]
allPairs w l = [(x,y) | x <- w, y <- l]

-- return the elements from postitions given the indexes https://github.com/EthanDY/cs312_project
matchIndexPos :: Int -> [Int] -> [a] -> [a]
matchIndexPos a indexes positions
    | a /= length indexes = positions !! (indexes !! a) : matchIndexPos (a+1) indexes positions
    | otherwise = []

-- create a list of random unique numbers from 0 to (width * length -1) https://github.com/EthanDY/cs312_project
randomIndexes :: Int -> Int -> Int -> IO [Int]
randomIndexes width length num =
    do
        g <- newStdGen
        return $ take num $ nub $ randomRs (0, width * length - 1 :: Int) g

-- Function to create a random mine positions list https://github.com/EthanDY/cs312_project
randomPositions :: Int -> Int -> Int -> IO [(Int, Int)]
randomPositions width length num = 
    do
        indexes <- randomIndexes width length num
        return $ matchIndexPos 0 indexes $ allPairs [0..width-1] [0..length-1]

-- Generate a row of game board with characters 
-- modified from https://github.com/EthanDY/cs312_project
generateRow :: [Grid] -> String
generateRow [] = []
generateRow (h : t) 
    | mined h = " X " ++ generateRow t
    | flagged h = " F " ++ generateRow t
    | otherwise = " - " ++ generateRow t

-- Generate the game board with characters https://github.com/EthanDY/cs312_project
generateBoard :: [[Grid]] -> Int -> String
generateBoard [] _ = []
generateBoard (h : t) c
    | c < 10 = (show c) ++ " " ++  generateRow h ++ "\n" ++ generateBoard t (c+1)
    | otherwise = (show c) ++ generateRow h ++ "\n" ++ generateBoard t (c+1) 

-- Generate column coordinates of the board https://github.com/EthanDY/cs312_project
generateColumnCoord :: Foldable t => [t a] -> Int -> [Char]
generateColumnCoord [] _ = []
generateColumnCoord board c
    | c == 0 = "   " ++ (show c) ++ generateColumnCoord board (c+1)
    | c < length (board!!0) && c < 10 = "  " ++ (show c) ++ generateColumnCoord board (c+1)
    | c < length (board!!0) && c >= 10 = " " ++ (show c) ++ generateColumnCoord board (c+1)
    | otherwise = []

-- Hide mine of game board 
-- modified https://github.com/EthanDY/cs312_project
generateRow_hide :: [Grid] -> String
generateRow_hide [] = []
generateRow_hide (h : t) 
    | flagged h = " F " ++ generateRow_hide t
    | otherwise = " - " ++ generateRow_hide t

-- Hide mine of game board https://github.com/EthanDY/cs312_project
generateBoard_hide :: [[Grid]] -> Int -> String
generateBoard_hide [] _ = []
generateBoard_hide (h : t) c
    | c < 10 = (show c) ++ " " ++  generateRow_hide h ++ "\n" ++ generateBoard_hide t (c+1)
    | otherwise = (show c) ++ generateRow_hide h ++ "\n" ++ generateBoard_hide t (c+1)

-- gerating game board https://github.com/EthanDY/cs312_project
generateSingleRow_hide :: [Grid] -> String
generateSingleRow_hide [] = []
generateSingleRow_hide (h : t) 
    | flagged h = " F " ++ generateSingleRow_hide t
    | reached h && num h /= 0 = "[" ++ show (num h) ++ "]" ++ generateSingleRow_hide t
    | reached h && num h == 0 = "[ ]" ++ generateSingleRow_hide t
    | otherwise = " - " ++ generateSingleRow_hide t

-- gerating game board https://github.com/EthanDY/cs312_project
generateSingleBoard_hide :: [[Grid]] -> Int -> String
generateSingleBoard_hide [] _ = []
generateSingleBoard_hide (h : t) c
    | c < 10 = (show c) ++ " " ++  generateSingleRow_hide h ++ "\n" ++ generateSingleBoard_hide t (c+1)
    | otherwise = (show c) ++ generateSingleRow_hide h ++ "\n" ++ generateSingleBoard_hide t (c+1)

-- gerating game board https://github.com/EthanDY/cs312_project
generateSingleRow :: [Grid] -> String
generateSingleRow [] = []
generateSingleRow (h : t) 
    | mined h = " X " ++ generateSingleRow t
    | flagged h = " F " ++ generateSingleRow t
    | reached h && num h /= 0 = "[" ++ show (num h) ++ "]" ++ generateSingleRow t
    | reached h && num h == 0 = "[ ]" ++ generateSingleRow t
    | otherwise = " - " ++ generateSingleRow t

-- gerating game board https://github.com/EthanDY/cs312_project
generateSingleBoard :: [[Grid]] -> Int -> String
generateSingleBoard [] _ = []
generateSingleBoard (h : t) c
    | c < 10 = (show c) ++ " " ++  generateSingleRow h ++ "\n" ++ generateSingleBoard t (c+1)
    | otherwise = (show c) ++ generateSingleRow h ++ "\n" ++ generateSingleBoard t (c+1) 


-- update the row of game board after clicking https://github.com/EthanDY/cs312_project
clickRow :: Int -> [Grid] -> Int -> [Grid]
clickRow _ [] _ = []
clickRow x (h : t) c
    | c == x = Grid {location = location h, mined = mined h, reached = True, flagged = False, num = num h} : t
    | otherwise = h : clickRow x t (c + 1)

-- Click a grid and update the whole game board https://github.com/EthanDY/cs312_project
click :: Int -> Int -> [[Grid]] -> Int -> [[Grid]]
click _ _ [] _ = []
click x y (h : t) c
    | c == y = clickRow x h 0 : t                 
    | otherwise = h : click x y t (c + 1)

-- 1.1 Determine whether (x,y) is flagged
flagCheck :: Int -> Int -> [[Grid]] -> Bool
flagCheck x y board = flagged (findGrid x y board)

-- 1.1 Determine whether (x,y) is reached
reachedCheck :: Int -> Int -> [[Grid]] -> Bool
reachedCheck x y board = reached (findGrid x y board)

-- 1.1 Count the flags on board
countFlagsRow :: [Grid] -> Int
countFlagsRow [] = 0
countFlagsRow (h : t)
    | flagged h = 1 + countFlagsRow t
    | otherwise = countFlagsRow t

-- 1.1 Count the flags on board
countFlags :: [[Grid]] -> Int
countFlags [] = 0
countFlags (h : t) = countFlagsRow h + countFlags t

-- 1.1 Count the reached grids on board
countReachedRow :: [Grid] -> Int
countReachedRow [] = 0
countReachedRow (h : t)
    | reached h = 1 + countReachedRow t
    | otherwise = countReachedRow t

-- 1.1 Count the reached grids on board
countReached :: [[Grid]] -> Int
countReached [] = 0
countReached (h : t) = countReachedRow h + countReached t

-- Check if the player win the game https://github.com/EthanDY/cs312_project
singleWinRowCheck :: [Grid] -> Int -> Int -> Bool
singleWinRowCheck [] _ _ = True
singleWinRowCheck (h : t) c length
    | c < length = ((mined h && not (reached h)) || (not (mined h) && reached h)) && singleWinRowCheck t (c + 1) length
    | otherwise = True

-- Check if the player win the game https://github.com/EthanDY/cs312_project
singleWinBoardCheck :: [[Grid]] -> Int -> Int -> Int -> Bool
singleWinBoardCheck [] _ _ _= True
singleWinBoardCheck (h : t) c width length
    | c < width = singleWinRowCheck h 0 length && singleWinBoardCheck t (c + 1) width length
    | otherwise = True

-- Check if the player lose the game https://github.com/EthanDY/cs312_project
singleLoseRowCheck :: [Grid] -> Int -> Int -> Bool
singleLoseRowCheck [] _ _ = False
singleLoseRowCheck (h : t) c length
    | c < length = (mined h && reached h) || singleLoseRowCheck t (c + 1) length
    | otherwise = False

-- Check if the player lose the game https://github.com/EthanDY/cs312_project   
singleLoseBoardCheck :: [[Grid]] -> Int -> Int -> Int -> Bool
singleLoseBoardCheck [] _ _ _= False
singleLoseBoardCheck (h : t) c width length
    | c < width = singleLoseRowCheck h 0 length || singleLoseBoardCheck t (c + 1) width length
    | otherwise = False

-- Check if all grid on the same row with mines have flag on it https://github.com/EthanDY/cs312_project
allMinesOnFlagRow :: [Grid] -> Bool
allMinesOnFlagRow [] = True
allMinesOnFlagRow (h:t)
    | mined h && mynot (flagged h) = False
    | mynot (mined h) && flagged h = False
    | otherwise = True && allMinesOnFlagRow t

-- Check if all grid with mines have flag on it https://github.com/EthanDY/cs312_project
allMinesOnFlag :: [[Grid]] -> Bool
allMinesOnFlag [] = True
allMinesOnFlag (h:t) = allMinesOnFlagRow h && allMinesOnFlag t

-- expand https://github.com/EthanDY/cs312_project
expandUp :: Int -> Int -> [[Grid]] -> [[Int]]
expandUp row column board
    | row < 0 || mined (findGrid row column board) == True || reached (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandUp (row - 1) (column) board
-- https://github.com/EthanDY/cs312_project
expandDown :: Int -> Int -> [[Grid]] -> [[Int]]
expandDown row column board
    | row >= length board || mined (findGrid row column board) == True || reached (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandDown (row + 1) (column) board
-- https://github.com/EthanDY/cs312_project
expandLeft :: Int -> Int -> [[Grid]] -> [[Int]]
expandLeft row column board
    | column < 0 || mined (findGrid row column board) || reached (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandLeft row (column - 1) board
-- https://github.com/EthanDY/cs312_project
expandRight :: Int -> Int -> [[Grid]] -> [[Int]]
expandRight row column board
    | column >= length (board!!0) || mined (findGrid row column board) == True || reached (findGrid row column board) == True = []
    | num (findGrid row column board) /= 0 || flagged (findGrid row column board) == True = [[row, column]]
    | otherwise = [row, column] : expandRight row (column + 1) board
-- https://github.com/EthanDY/cs312_project
expandUDRL :: Int -> Int -> [[Grid]] -> [[Int]]
expandUDRL row column board = nub (expandUp row column board ++ expandDown row column board ++ expandLeft row column board ++ expandRight row column board)
-- https://github.com/EthanDY/cs312_project
expandArea :: [[Int]] -> [[Int]] -> [[Grid]] -> [[Int]]
expandArea _ [] _  = []
expandArea lst (h:t) board
    | mined (findGrid (h!!0) (h!!1) board) == True = (h:t)
    | (h:t) == [] = []
    | otherwise = let toExpand = nub (expand (h:t) board)
                      expanded = nub (h:lst)
                    in h : expandArea expanded (toExpand \\ expanded) board
-- https://github.com/EthanDY/cs312_project
expand :: [[Int]] -> [[Grid]] -> [[Int]]
expand [] _ = []
expand lst board = nub (foldr (\x y -> y ++ expandUDRL (x!!0) (x!!1) board) [] lst)
-- https://github.com/EthanDY/cs312_project
expandAreaClick :: Foldable t => t [Int] -> [[Grid]] -> [[Grid]]
expandAreaClick lst board = foldr (\x y -> click (x!!1) (x!!0) y 0) board lst

-- deln n e list function removes first n occurrences of e in list https://github.com/EthanDY/cs312_project
deln :: (Num t, Ord t, Eq a) => t -> a -> [a] -> [a]
deln n e [] = []
deln n e (h:t)
    | n == 0 = h:t
    | (n > 0) && (e == h) = deln (n-1) e t
    | otherwise = h : deln n e t

-- Return the grid with given coordinate https://github.com/EthanDY/cs312_project
findGrid :: Int -> Int -> [[a]] -> a
findGrid row column board = board !! row !! column

-- Change a tuple to list https://github.com/EthanDY/cs312_project
tupeToList :: (a, a) -> [a]
tupeToList t = firTup t : secTup t : []

-- Get the number of mines around the grid https://github.com/EthanDY/cs312_project
getGridNum :: Grid -> [[Grid]] -> Int
getGridNum grid board = let (x,y) = location grid
                        in mineToOne (x-1) (y-1) board + mineToOne (x-1) y board + mineToOne (x-1) (y+1) board  +
                        mineToOne x (y-1) board + mineToOne x (y+1) board +
                        mineToOne (x+1) (y-1) board + mineToOne (x+1) y board + mineToOne (x+1) (y+1) board

-- Update the column of a row's grids to show the number of mines around it https://github.com/EthanDY/cs312_project
updateGridNumColumn :: [Grid] -> [[Grid]] -> [Grid]
updateGridNumColumn [] _ = []
updateGridNumColumn (h : t) board
    = Grid {location = location h, mined = mined h,reached = reached h, flagged = flagged h, num = getGridNum h board} : updateGridNumColumn t board

-- Get a new board with all grids' num updated https://github.com/EthanDY/cs312_project 
updateGridNum :: [[Grid]] -> [[Grid]] -> [[Grid]]
updateGridNum [] _ = []
updateGridNum (h:t) board
    = updateGridNumColumn h board : updateGridNum t board

-- Check if the grid with current coordinate is mined, if so return 1. https://github.com/EthanDY/cs312_project
mineToOne :: Num p => Int -> Int -> [[Grid]] -> p
mineToOne x y board
    | x < 0 || y < 0 || x >= length (board!!0) || y >= length board = 0
    | mined (findGrid x y board) == True = 1
    | otherwise = 0

-- myhead returns the head of the list https://github.com/EthanDY/cs312_project
myhead (h:t) = h

-- mytail returns the tail of the list https://github.com/EthanDY/cs312_project
mytail (h:t) = t

-- reverseFixdel returns the string after applying '\DEL' but in reverse order https://github.com/EthanDY/cs312_project
reverseFixdel [] = []
reverseFixdel lst = foldl (\x y -> if x /= [] && y == '\DEL' then mytail x else y:x) [] lst

-- fixdel get user input and reverse the order of string got from reverseFixdel https://github.com/EthanDY/cs312_project
fixdel :: IO String
fixdel =
    do 
        lst <- getLine 
        return (foldl (\x y -> y : x) [] (reverseFixdel lst) :: String)

-- Get a valid number from user in range [bot, upp] https://github.com/EthanDY/cs312_project
getValidInput :: (Num b, Ord b, Read b) => b -> b -> IO b
getValidInput bot upp =
    do
        input <- fixdel
        if input == "-1" && bot == -1 then return (-1)
        else if isNum input == False then
            do
                putStrLn "Please enter a valid input"
                getValidInput bot upp
                else 
                    do
                        let num = read input
                        if num >= bot && num <= upp
                            then
                            return num
                            else do
                                putStrLn "Please enter a valid input"
                                getValidInput bot upp
        
-- Check if the string can be converted to a positive number https://github.com/EthanDY/cs312_project
isNum [] = True
isNum (h:t) = isDigit h && isNum t

-- Set/Cancel a flag https://github.com/EthanDY/cs312_project
setFlagRow :: Int -> [Grid] -> Int -> [Grid]
setFlagRow _ [] _ = []
setFlagRow x (h : t) c
    | c == x = Grid {location = location h, 
    mined = mined h, reached = reached h, flagged = mynot (flagged h), num = num h} : t
    | otherwise = h : setFlagRow x t (c + 1)

-- https://github.com/EthanDY/cs312_project
setFlag :: Int -> Int -> [[Grid]] -> Int -> [[Grid]]
setFlag _ _ [] _ = []
setFlag x y (h : t) c
    | c == y = setFlagRow x h 0 : t                 
    | otherwise = h : setFlag x y t (c + 1)

-- 1.1 add the type declaration of numMines and grids
singleGameLoop :: [[Grid]] -> String -> String -> UTCTime -> Int -> Int -> IO()
-- 1.1 add numMines and grids
singleGameLoop board diffString name time numMines grids
    | allMinesOnFlag board = do
        putStrLn(generateColumnCoord board 0)
        putStrLn(generateSingleBoard board 0)
        finishTime <- getCurrentTime
        let (seconds, _) = properFraction (diffUTCTime finishTime time)
        putStrLn ("Clear Time: " ++ show seconds ++ "s")
        putStrLn("You Win!!! \n")
        writeRecords diffString name seconds
        putStrLn("Continue?")
        putStrLn("1. Yes    2. No")
        choose <- getValidInput 1 2
        if choose == 1 then initGame
        else if choose == 2 then return ()
        else putStrLn "Invalid input, game ended."
    | singleWinBoardCheck board 0 (length (board!!0)) (length board) = do
        putStrLn(generateColumnCoord board 0)
        putStrLn(generateSingleBoard board 0)
        finishTime <- getCurrentTime
        let (seconds, _) = properFraction (diffUTCTime finishTime time)
        putStrLn ("Clear Time: " ++ show seconds ++ "s")
        putStrLn("You Win!!! \n")
        writeRecords diffString name seconds
        putStrLn("Continue?")
        putStrLn("1. Yes    2. No")
        choose <- getValidInput 1 2
        if choose == 1 then initGame
        else if choose == 2 then return ()
        else putStrLn "Invalid input, game ended."
    | singleLoseBoardCheck board 0 (length (board!!0)) (length board) = do
        putStrLn(generateColumnCoord board 0)
        putStrLn(generateSingleBoard board 0)
        putStrLn("You Lose... \n")
        putStrLn("Continue?")
        putStrLn("1. Yes    2. No")
        choose <- getValidInput 1 2
        if choose == 1 then initGame
        else if choose == 2 then return ()
        else putStrLn "Invalid input, game ended."
    | otherwise = do
                    -- 1.1 make the upper bound of the board separated from the user's input
                    putStrLn("\n" ++ generateColumnCoord board 0)
                    putStrLn(generateSingleBoard_hide board 0)
                    -- 1.1 show the number of flags and unreached grids left
                    putStrLn("Flags: " ++ show (numMines - countFlags board))
                    putStrLn("Uncovered cells: " ++ show (grids - countFlags board - countReached board))
                    three <- setCancelClick board numMines
                    if head three == -1 
                        then saveGame board diffString name time numMines grids
                        else do
                            let lst = expandArea [] [take 2 three] board              
                            let finalboard = if three !! 2 == 1
                                                then 
                                                setFlag (three !! 1) (three !! 0) board 0
                                                else expandAreaClick lst board
                            putStrLn("Loading... \n")
                            singleGameLoop finalboard diffString name time numMines grids

-- 1.1 separated from singleGameLoop
setCancelClick :: [[Grid]] -> Int -> IO [Int]
setCancelClick board numMines = 
    do 
        -- 1.2 quit
        putStrLn "\nset/cancel flag? 1.Yes 2.No (0 to quit)"
        flagAns <- getValidInput 0 2
        if flagAns == 0
            then return [-1]
            else do
                putStrLn "Enter Row:"
                row <- getValidInput 0 ((length board) -1)
                putStrLn "Enter Column :"
                column <- getValidInput 0 (length (board!!0)-1) 
                if not (flagCheck row column board) && (numMines - countFlags board) == 0 && flagAns == 1
                    then do 
                        putStrLn("\nYou can only cancel an existing flag because you have no flag left")
                        setCancelClick board numMines
                    -- 1.1 bug fixed
                    else if (reachedCheck row column board) && flagAns == 1
                        then do
                            putStrLn("\nPlease choose an unreached ground to set your flag")
                            setCancelClick board numMines
                            else do
                                return [row, column, flagAns]

-- 1.2 save the game
saveGame :: [[Grid]] -> String -> String -> UTCTime -> Int -> Int -> IO ()
saveGame board diffString name time numMines grids =
    do
        putStrLn ("Save the game? 1.Yes 2.No")
        saveAns <- getValidInput 1 2
        if saveAns == 2
            then initGame
            else do
                writeToFile board diffString name time numMines grids
                putStrLn ("Game saved to " ++ name ++ ".txt. Please remember the file name for resuming game!\n")
                initGame

-- Start the game modified
initGame :: IO ()
initGame =
    do
        createFileIfNonExist
        choose <- usrRanking
        if choose == 4 then return()
        else if choose == 2 
            then
                startSingleGame
        else do
                putStrLn "File name: ____.txt?"
                fileName <- fixdel
                catch (readFromFile fileName) (\ (SomeException e) ->  initGame)



-- Check if user wants to see the ranking https://github.com/EthanDY/cs312_project
usrRanking :: IO Integer
usrRanking = 
    do
        -- 1.2 
        putStrLn ("1. Check Ranking 2. Play Game 3. Load Game 4. Quit")
        choose <- getValidInput 1 4
        if choose == 1 then do
            generateRanking "SuperEasy"
            generateRanking "Easy"
            generateRanking "Medium"
            generateRanking "Difficult"
            usrRanking
        else if choose == 2 then return 2
        else if choose == 3 then return 3
        else return 4

-- Start game https://github.com/EthanDY/cs312_project
startSingleGame :: IO ()
startSingleGame = 
    do
        putStrLn "Please enter your name:"
        name <- fixdel
        putStrLn "Please choose difficulty:"
        putStrLn "0. Super Easy 1. Easy   2. Medium   3. Difficult"
        diff <- getValidInput 0 3
        let diffString = if diff == 1
            then "Easy"
            else if diff == 2
                then "Medium"
                else if diff == 3
                    then "Difficult"
                    else "SuperEasy"
        let size = if diff == 1
            then 9
            else if diff == 2
                then 18
                else if diff == 3
                    then 24
                    else 5
        let numMines = if diff == 1
            then 10
            else if diff == 2
                then 40
                else if diff == 3
                    then 99
                    else 2
        mapMinePos <- randomPositions size size numMines
        let genBoard = buildBoard 0 0 size size mapMinePos
        time <- getCurrentTime
        -- passing the numMines to singleGameLoop
        game <- singleGameLoop (updateGridNum genBoard genBoard) diffString name time numMines (size * size)
        return game

-- Convert a list of strings to a list of records https://github.com/EthanDY/cs312_project
stringLstToRecordLst [] = []
stringLstToRecordLst (h:t) = let rs = words h
                                in (Record (rs!!0) (rs!!1) (read (rs!!2))) : stringLstToRecordLst t

-- Create a string with the records' name and time https://github.com/EthanDY/cs312_project
cRank [] _ = []
cRank (h:t) c = (show c) ++ ". " ++ name h ++ findSpaceNum (length (name h)) ++ show (time h) ++ "s\n" ++ cRank t (c+1)

-- Find proper number of space character https://github.com/EthanDY/cs312_project
findSpaceNum n 
    | 9 - n > 0 = " " ++ findSpaceNum (n+1)
    | otherwise = []

-- Generate a ranking form https://github.com/EthanDY/cs312_project
generateRanking difficulty= 
    do
        content <- readFile (difficulty ++ ".txt")
        let allLines = lines content
        let recordLst = stringLstToRecordLst allLines
        let sortedRecords = sort recordLst
        let title = if difficulty == "SuperEasy"
                    then "Super Easy"
                    else difficulty
        putStrLn ("  " ++ title)
        putStrLn ("----------------")
        putStrLn (cRank sortedRecords 1)

-- Write records to file https://github.com/EthanDY/cs312_project
writeRecords difficulty name time =
    do
        let diffString = if difficulty == "Super Easy"
                        then "SuperEasy"
                        else difficulty
        let record = Record diffString name time
        content <- readFile (diffString ++ ".txt")
        let allLines = lines content
        let recordLst = stringLstToRecordLst allLines
        let newLst = record : recordLst
        let sortedRecords = take 5 (sort newLst)
        when (length content >= 0) (writeFile (diffString ++ ".txt") $ recordsToString sortedRecords)

-- Change a list of records into a string to write to file https://github.com/EthanDY/cs312_project
recordsToString [] = []
recordsToString (h:t) = (difficulty h) ++ " "  ++ (name h) ++ " " ++ show (time h) ++ "\n" ++ recordsToString t

-- Check if the current directory has these files, if not create them https://github.com/EthanDY/cs312_project
createFileIfNonExist = 
    do
        se <- doesFileExist "SuperEasy.txt"
        e <- doesFileExist "Easy.txt"
        m <- doesFileExist "Medium.txt"
        d <- doesFileExist "Difficult.txt"
        when (se == False) (writeFile "SuperEasy.txt" "")
        when (e == False) (writeFile "Easy.txt" "")
        when (m == False) (writeFile "Medium.txt" "")
        when (d == False) (writeFile "Difficult.txt" "")

-- 1.2 convert a board to a list of grids' data
boardToLst :: [[Grid]] -> [String]
boardToLst [] = []
boardToLst (h : t) = boardToLstRow h ++ boardToLst t

-- 1.2 covert a row to a list of grids' data
boardToLstRow :: [Grid] -> [String]
boardToLstRow [] = []
boardToLstRow (h : t) = (gridToString h) : (boardToLstRow t)

-- 1.2 convert a grid's data to a string
gridToString :: Grid -> String
gridToString h =
    (show (firTup (location h))) ++ " " ++ (show (secTup (location h))) ++ " " ++ (booleanToString (mined h)) ++ " " ++ (booleanToString (reached h)) ++ " " ++ (booleanToString (flagged h)) ++ " " ++ (show (num h))

-- 1.2 convert a Boolean to a String
booleanToString True = "True"
booleanToString False = "False"

-- 1.2 convert a String to a Boolean
stringToBoolean :: String -> Bool
stringToBoolean s = 
    if s =="True" then True else False

-- 1.2 write the game data to file
writeToFile board diffString name time numMines grids =
    do
        let boardLst = boardToLst board
        writeFile (name ++ ".txt") $ (allToString boardLst diffString name time numMines grids)

-- 1.2 covert all saving data to lines of strings
allToString boardLst diffString name time numMines grids = 
    diffString ++ " " ++ name ++ " " ++ (show (time)) ++ " " ++ (show (numMines)) ++ " " ++ (show (grids)) ++ "\n" ++ boardLstToString boardLst

-- 1.2 cover boardLst to lines of strings
boardLstToString [] = ""
boardLstToString (h : t) = h ++ "\n" ++ (boardLstToString t)

-- 1.2 handling exception when readFile does not have a proper fileName
readHandler :: IOError -> IO String
readHandler e  
    | isDoesNotExistError e = do putStrLn "The file does not exist, try again." ; return ""
    | otherwise = do putStrLn "Something went wrong, try again." ; return ""

-- 1.2 read the saved file to game
readFromFile fileName =
    do
        content <- readFile (fileName ++ ".txt") `catch` readHandler
        let allLines = lines content
        let mainLine = head allLines
        let gridLines = tail allLines
        let gridLst = lstToGrids gridLines
        let rs = words mainLine
        let diffString = (rs!!0)
        let name = (rs!!1)
        let time = ((read ((rs!!2) ++ " " ++ (rs!!3) ++ " " ++ (rs!!4))::UTCTime))
        let numMines = (read (rs!!5))
        let grids = (read (rs!!6))
        let board = if diffString == "SuperEasy"
            then chunksOf 5 gridLst
            else if diffString == "Easy"
                then chunksOf 9 gridLst
                    else if diffString == "Medium"
                        then chunksOf 18 gridLst
                            else chunksOf 24 gridLst
        singleGameLoop board diffString name time numMines grids

-- 1.2 convert a list of strings to a list of grids
lstToGrids :: [String] -> [Grid]
lstToGrids [] = []
lstToGrids (h : t) = let rs = words h 
                        in (Grid ((read(rs!!0)),(read(rs!!1))) (stringToBoolean (rs!!2)) (stringToBoolean (rs!!3)) (stringToBoolean (rs!!4)) (read (rs!!5))) : (lstToGrids t)
