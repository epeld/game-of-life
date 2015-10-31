module GameOfLife where
import Histogram exposing (Histogram, histogram, occurances)
import FunctionUtils exposing (iterate, ignore1)

import Signal
import List
import List.Extra exposing (andThen)
import Maybe
import Set exposing (Set)


type alias Cell = (Int, Int)
type alias Generation = Set Cell


cells : List Cell -> Generation
cells = Set.fromList


signal : Generation -> Signal a -> Signal Generation
signal g s = Signal.foldp (ignore1 generation) g s


generations : Int -> Generation -> Generation
generations n g = iterate n generation g


generation : Generation -> Generation
generation xs = 
    let 
        remain = Set.filter (\x -> occurances x hist == 2) candidates
        born = Set.filter (\x -> occurances x hist > 2) candidates

        candidates = Histogram.keys hist
        hist = neighborHisto xs
    in
        Set.union born (Set.intersect remain xs)


neighborHisto : Set Cell -> Histogram Cell
neighborHisto cells = histogram <| List.concatMap neighbors <| Set.toList cells


neighbors : Cell -> List Cell
neighbors (x, y) = let offsets = [-1, 0, 1]
                       cells = offsets `andThen` \i ->
                               offsets `andThen` \j ->
                               [(x + i, y + j)]

                   in List.filter (\a -> a /= (x,y)) cells

