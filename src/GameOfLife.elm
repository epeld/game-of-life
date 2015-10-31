module GameOfLife where
import Histogram exposing (Histogram, histogram, keysWithCount)
import FunctionUtils exposing (iterate, ignore1)

import Signal
import List
import List.Extra exposing (andThen)
import Maybe
import Set exposing (Set, diff, union, intersect)


type alias Cell = (Int, Int)
type alias Generation = Set Cell


fromCells : List Cell -> Generation
fromCells = Set.fromList


toCells : Generation -> List Cell
toCells = Set.toList


signal : Generation -> Signal a -> Signal Generation
signal g s = Signal.foldp (ignore1 generation) g s


generations : Int -> Generation -> Generation
generations n g = iterate n generation g


{-
From wikipedia:

For each new generation,

1. Any live cell with fewer than two live neighbours dies, as if caused by under-population.
2. Any live cell with two or three live neighbours lives on to the next generation.
3. Any live cell with more than three live neighbours dies, as if by over-population.
4. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

-}
generation : Generation -> Generation
generation g = 
    let 
        histo = neighborHisto g

        two = keysWithCount 2 histo
        three = keysWithCount 3 histo
    in
        -- Rule 2: living cells with count 2 live 
        -- Rule 2 & 4: cells with count 3 live
        -- Rule 1 & 4: all else die
        (g `intersect` two) `union` three


neighborHisto : Set Cell -> Histogram Cell
--neighborHisto cells = histogram <| List.concatMap neighbors <| Set.toList cells
neighborHisto cells = 
    Set.toList cells 
    |> List.concatMap neighbors 
    |> histogram


neighbors : Cell -> List Cell
neighbors (x, y) = let
                       offsets = [-1, 0, 1]
                       cells = offsets `andThen` \i ->
                               offsets `andThen` \j ->
                               [(x + i, y + j)]

                   in
                      List.filter (\a -> a /= (x,y)) cells

