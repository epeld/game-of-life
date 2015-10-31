module Box where

import Maybe exposing (andThen)
import String
import List.Extra as L

import Point exposing (..)


type Box number = Box number number number number


contains : Box number -> (number, number) -> Bool
contains (Box bx by w h) (x,y) = 
    bx <= x && 
    by <= y && 
    x <= bx + w &&
    y <= by + h


-- Stringify in a way consistent with svg attributes, e.g viewBox
string : Box number -> String
string (Box x y w h) = List.map toString [x, y, w, h] |> String.join " "


boundingBox : List Point -> Maybe (Box number)
boundingBox pts =
    let
        xs = List.map x pts
        ys = List.map y pts
    in
       List.minimum xs `andThen` \minx ->
       List.maximum xs `andThen` \maxx ->
       List.minimum ys `andThen` \miny ->
       List.maximum ys `andThen` \maxy -> 
           Just (Box minx miny (maxx - minx) (maxy - miny))


points : Box Int -> List (number, number)
points (Box x y w h) = 
    [x..x+w] `L.andThen` \i ->
    [y..y+h] `L.andThen` \j ->
    [(i,j)]


{-
enlarge : Float -> List Float -> List Float
enlarge k [x,y,w,h] =
    let
        diff = k - 1
    in
        [x - diff * w / 2, y - diff * h / 2, w * diff, h * diff]
-}
