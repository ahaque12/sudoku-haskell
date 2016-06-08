module Sudoku where

import Data.List
import Data.Char
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
blank (Sudoku rs) = (fst tuple, fst . snd $ tuple) where 
    tuple = head . dropWhile (\(_,x) -> x==[]) . zip [1..9::Int] $ head . map (dropWhile (\(_,x) -> x/=Nothing)) .zip [1..9::Int] $ rs

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) = undefined

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update = undefined

-------------------------------------------------------------------------

solve :: Sudoku -> Maybe Sudoku
solve s | undefined = Nothing  -- There's a violation in s
        | undefined = Just s   -- s is already solved
        | otherwise = pickASolution possibleSolutions
  where
    nineUpdatedSuds   = undefined :: [Sudoku]
    possibleSolutions = [solve s' | s' <- nineUpdatedSuds]

pickASolution :: [Maybe Sudoku] -> Maybe Sudoku
pickASolution suds = undefined

