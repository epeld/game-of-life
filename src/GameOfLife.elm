module GameOfLife where
import Histogram exposing (Histogram, histogram, keysWithCount)
import FunctionUtils exposing (iterate, ignore1)
import Box exposing (Box)

import Signal
import List
import List.Extra exposing (andThen)
import Maybe
import Set exposing (Set, diff, union, intersect)


type alias Cell = (Int, Int)
type alias Generation = Set Cell
type alias ClipBox = Box Int


type Update = Next | Add Cell | Remove Cell


signal : Maybe ClipBox -> Generation -> Signal Update -> Signal Generation
signal b g s = Signal.foldp (update b) g s


contains = Set.member

update : Maybe ClipBox -> Update -> Generation -> Generation
update b u g = case u of
    Next -> next b g
    Add c -> Set.insert c g
    Remove c -> Set.remove c g


next : Maybe ClipBox -> Generation -> Generation
next b g = generation (clip b g)


fromCells : List Cell -> Generation
fromCells = Set.fromList


toCells : Generation -> List Cell
toCells = Set.toList


generations : Int -> Generation -> Generation
generations n g = iterate n generation g


clip : Maybe ClipBox -> Generation -> Generation
clip b = case b of
    Nothing -> identity
    Just b' -> clipped b'


clipped : ClipBox -> Generation -> Generation
clipped box g = filter (Box.contains box) g


filter = Set.filter

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

