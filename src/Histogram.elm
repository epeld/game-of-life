module Histogram where
import Dict 
import List
import Maybe

import List.Extra exposing (group, zip)

type alias Histogram a = Dict.Dict a Int

histogram : List comparable -> Histogram comparable
histogram xs = 
    let groups = group (List.sort xs)
        assocs = zip (List.filterMap List.head groups) (List.map List.length groups)
     in Dict.fromList assocs


occurances : comparable -> Histogram comparable -> Int
occurances x hst = Maybe.withDefault 0 <| Dict.get x hst
