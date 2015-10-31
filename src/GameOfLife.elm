module GameOfLife where
import Histogram exposing (Histogram, histogram, occurances)
import FunctionUtils exposing (iterate)

import List
import List.Extra exposing (andThen)
import Maybe
import Set exposing (Set)


type alias Cell = (Int, Int)
type alias Generation = Set Cell


generations : Int -> Generation -> Generation
generations n g = iterate n generation g


-- Utility function
generationList : List Cell -> List Cell
generationList = Set.toList << generation << Set.fromList


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

