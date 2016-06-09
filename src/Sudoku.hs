module Sudoku (Sudoku, readSudoku, printSudoku, solve) where

import Data.List
import Data.Char
import Data.Maybe
import Test.QuickCheck

-------------------------------------------------------------------------

data Sudoku = Sudoku [[Maybe Int]] deriving(Show,Eq)

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing 

isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rs) = validDigits && validNumCols && validNumRows
    where
        validDigits = and . map and $ map (map cond) rs where
            cond :: (Maybe Int) -> Bool
            cond Nothing = True
            cond x = (x <= Just 9) && (x >= Just 1)
        validNumCols = and . map (\x -> length x == 9) $ rs
        validNumRows = length rs == 9

isSolved :: Sudoku -> Bool
isSolved (Sudoku rs) = and . map and $ map (map (/= Nothing)) rs

-------------------------------------------------------------------------

printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rs) = mapM_ putStrLn $ map stringFunc rs where
    stringFunc :: [Maybe Int] -> String
    stringFunc []           = ""
    stringFunc (Just x:xs)  = intToDigit x : stringFunc xs
    stringFunc (Nothing:xs) = '.' : stringFunc xs

readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
                    file <- readFile fp
                    return $ Sudoku $ map strToRow $ lines file
    where
        strToRow = map conv 
        conv '.' = Nothing
        conv x   = Just $ digitToInt x

-------------------------------------------------------------------------

cell :: Gen (Maybe Int) 
cell = frequency [(1::Int,elements (fmap Just [1..9])),(9::Int,elements [Nothing])] 

instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku x = isSudoku x

-------------------------------------------------------------------------

type Block = [Maybe Int] 

isOkayBlock :: Block -> Bool
isOkayBlock x = fst $ foldl checkDup (True, Nothing) $ dropWhile (==Nothing) $ sort x
    where 
        checkDup (a,b) x = (a && b/=x,x)

blocks :: Sudoku -> [Block] 
blocks (Sudoku rs) = rs ++ transpose rs ++ sqrBlock rs where
    sqrBlock rs = map (\x -> concatMap (take 3 . drop x) (take 3 . drop x $ rs)) [0,3,6]

isOkay :: Sudoku -> Bool
isOkay x = and . map isOkayBlock $ blocks x

-------------------------------------------------------------------------

type Pos = (Int,Int)

blank :: Sudoku -> Pos
blank (Sudoku rs) = (fst tuple, fst . head . snd $ tuple) where 
    tuple = head . dropWhile (\(_,x) -> x==[]) . zip [0..8::Int] $ map (dropWhile (\(_,x) -> x/=Nothing)) $ map (zip [0..8::Int]) rs

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (k,val) = let  
                     split = splitAt k xs
                     beg = fst split
                     end = tail . snd $ split
            in beg ++ val : end 

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku rs) (i,j) val = let
                     split = splitAt i rs
                     beg = fst split
                     mid = head . snd $ split
                     end = tail . snd $ split
            in Sudoku $ beg ++ mid !!= (j,val) : end
                                

-------------------------------------------------------------------------

-- Get column x from sudoku s (used in Assignment X below)
column :: Int -> Sudoku -> Block
column x s = transpose (rows s) !! x

-- Get row x from sudoku s (used in Assignment X below)
row :: Int -> Sudoku -> Block
row x s = (rows s) !! x

-- Get square (x,y), 0 <= x,y <= 2 from sudoku s
square :: (Int, Int) -> Sudoku -> Block
square (x,y) s = 
      concat
      $ [take 3 (drop (x*3) row) | row <- take 3 (drop (y*3) (rows s))]

-- This function calculates the number of free spaces in the given blocks
-- and zips it with the index
blanksInBlocks :: Sudoku -> [(Int,Int)]
blanksInBlocks s = [ (n, blanksInBlock b) | (b,n) <- zip (blocks s) [0..] ]
                        
blanksInBlock :: Block -> Int
blanksInBlock bl = length (filter (== Nothing) bl)

-- Propagate sudoku
-- Returns Nothing if not able to propagate, otherwise
-- 1) finds the first block with only 1 free spot
-- 2) calculates the index in block and corresponding value
propagate :: Sudoku -> Maybe Sudoku
propagate s | length availBlocks == 0 = Nothing
            | otherwise = propagateBlock s (fst $ head availBlocks) where
    availBlocks = filter (\p -> snd p == 1) (blanksInBlocks s) 
    propagateBlock s n | n < 9     = Just (propagateRow s n)
                       | n < 18    = Just (propagateColumn s (n-9))
                       | otherwise = Just (propagateSquare s (n-18)) where
        idx = missingInBlockIndex
        val = missingInBlock
        propagateRow s k    = update s (k,idx (row k s)) (Just (val (row k s)))
        propagateColumn s k = update s (idx (column k s),k) (Just (val (column k s)))
        propagateSquare s k = update s (y, x) (Just (val sq)) where
            sq = square (mod k 3, div k 3) s
            y  = 3*(div k 3) + (div (idx sq) 3)
            x  = 3*(mod k 3) + (mod (idx sq) 3)

-- Finds the index of the first "Nothing" in a block
missingInBlockIndex b = 
    snd $
    head $ 
    filter (\p -> fst p == Nothing) $
    zip b [0..8]

-- Calculates the value that should be instead of the "Nothing" in a block
missingInBlock :: Block -> Int
missingInBlock b = maxInBlock - (foldl (+) 0 (map (fromMaybe 0) b)) where
    maxInBlock = foldl (+) 0 [1..9]

-------------------------------------------------------------------------

solve :: Sudoku -> Maybe Sudoku
solve s | not (isSudoku s && isOkay s) = Nothing  -- There's a violation in s
        | isSolved s = Just s   -- s is already solved
        | propagated /= Nothing = solve $ fromJust propagated
        | otherwise = pickASolution possibleSolutions
  where
    propagated :: Maybe Sudoku
    propagated = Nothing -- propagate s
    nineUpdatedSuds   = [update s (blank s) i | i <- fmap Just [1..9::Int]] :: [Sudoku]
    possibleSolutions = [solve s' | s' <- nineUpdatedSuds]

pickASolution :: [Maybe Sudoku] -> Maybe Sudoku
pickASolution suds  
    | length xs == 0 = Nothing
    | otherwise      = head xs
    where
        xs = dropWhile (==Nothing) suds 
