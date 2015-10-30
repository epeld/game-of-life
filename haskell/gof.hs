module GameOfLife where
import Histogram

import Data.List
import Data.Maybe


type Cell = (Int, Int)


gen :: [Cell] -> [Cell]
gen xs = 
    let 
        remain = filter (\x -> occurances x hist == 2) candidates
        born = filter (\x -> occurances x hist > 2) candidates

        candidates = values hist
        hist = neighborHisto xs
    in
        (remain `intersect` xs) ++ born

neighborHisto :: [Cell] -> Histogram Cell
neighborHisto cells = histogram (concatMap neighbors cells)

neighbors :: Cell -> [Cell]
neighbors (x, y) = [(x + i, y + j) | i <- [-1, 0, 1], j <- [-1, 0, 1]] \\ [(x,y)]

