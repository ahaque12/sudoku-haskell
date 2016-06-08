
dist/build/sudoku/sudoku.exe : src/Sudoku.hs
    cabal build ---ghc-options="-O2"

