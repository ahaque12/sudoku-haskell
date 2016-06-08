
import Sudoku
import System.Environment
import Control.Monad

main :: IO ()
main = do
         args <- fmap head getArgs
         putStrLn "File Path..."
         putStrLn args
         putStrLn "Sudoku: "
         sud <- readSudoku args
         printSudoku sud
         putStrLn "Solving..."
         putStrLn "Solution: "
         let sol = solve sud
         printSudokuM sol where
    printSudokuM :: (Maybe Sudoku) -> IO ()
    printSudokuM Nothing = putStrLn "Nothing"
    printSudokuM (Just sud) = printSudoku sud

