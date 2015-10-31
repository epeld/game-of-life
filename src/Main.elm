module Main where
import GameOfLifeSvg
import GameOfLife

import Time
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (text)

main = 
    let
        initial = [(1,1), (2,1), (2,2), (1,0)]
        everySecond = Time.every Time.second
    in
        GameOfLifeSvg.signal (GameOfLife.fromCells initial) everySecond



{--
import FunctionUtils exposing (ignore1)
import Time
import Signal
import Graphics.Element exposing (show)
main = Signal.map show <| Signal.map toString <| Signal.foldp (ignore1 ((+) 1)) 0 <| Time.every Time.second
-}
