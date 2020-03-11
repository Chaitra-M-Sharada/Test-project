module Main where

import System.Environment

main = do args <- getArgs
          let 
            n   = read (args!!0) :: Int -- Boardsize to search
          print ("Number of Solutions = " ++ (show (sum (map length (nqueens n)))))

------------------------------------------------------------------------
-- nqueens
-- Find all possible queen placements by searching each column  
------------------------------------------------------------------------ 
nqueens :: Int -> [[[Int]]]
nqueens boardSize = map  (searchColumn boardSize) [1 .. boardSize]

------------------------------------------------------------------------
-- searchColumn 
-- Chose placement solutions that entail an initial placements in 
-- this column
------------------------------------------------------------------------
searchColumn :: Int -> Int -> [[Int]]
searchColumn boardSize row = 
  takeWhile (\a->head a == row) (pqueens boardSize row)

------------------------------------------------------------------------
-- pqueens
-- Try all possible placements of a queen in a given row on a board 
-- containing Boardsize postions, chosing only those
-- that aren't threatened by some other queen
------------------------------------------------------------------------
pqueens :: Int -> Int -> [[Int]]
pqueens boardSize row = queens boardSize where
  queens 0     = [[]]
  queens m = 
    [p ++ [n] | p <- queens (m-1), 
                n <- ([row..boardSize] ++[1..(row-1)]), 
                safe p n]

------------------------------------------------------------------------
-- safe
-- check whether any queen in the placement p threatens the possible 
-- position n
------------------------------------------------------------------------
safe :: [Int] -> Int -> Bool
safe p n = and [not (check (i, j) (m, n)) | (i, j) <- zip [1..(length p)] p]
	where m = length p + 1

------------------------------------------------------------------------
-- check
-- Two queens threaten each other is they're on the same row, the same 
-- column, or on a diagonal
------------------------------------------------------------------------
check :: (Int,Int) -> (Int,Int) -> Bool
check (c, l) (i, j) = (l==j) || (c+l == i+j)|| (c-l == i-j)
