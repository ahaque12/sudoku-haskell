all: dist/build/sudoku/sudoku.exe 

dist/build/sudoku/sudoku.exe: src/Sudoku.hs
	cabal build 

clean: 
	rm dist/build/sudoku/sudoku.exe
