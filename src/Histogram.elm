module Histogram where
import Dict 
import List
import Maybe
import Set exposing (Set)

import List.Extra exposing (group, zip)

type alias Histogram comparable = Dict.Dict comparable Int


filter = Dict.filter


keysWithCount : Int -> Histogram comparable -> Set comparable
keysWithCount n histo = filter (\_ c -> c == n) histo |> keys 


histogram : List comparable -> Histogram comparable
histogram xs = 
    let groups = group (List.sort xs)
        assocs = zip (List.filterMap List.head groups) (List.map List.length groups)
     in Dict.fromList assocs


keys : Histogram comparable -> Set comparable
keys hst = Set.fromList <| Dict.keys hst


occurances : comparable -> Histogram comparable -> Int
occurances x hst = Maybe.withDefault 0 <| Dict.get x hst
