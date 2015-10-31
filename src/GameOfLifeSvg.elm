module GameOfLifeSvg where

import Svg
import Svg.Attributes
import Maybe exposing (andThen, withDefault)
import String

import Point
import GameOfLife
import Polygon exposing (rect)


signal : GameOfLife.Generation -> Signal a -> Signal Svg.Svg
signal g s = Signal.map toSvgScene (GameOfLife.signal g s)


toSvgScene : GameOfLife.Generation -> Svg.Svg
toSvgScene g = Svg.svg [viewBox g] (toSvg g)


toSvg : GameOfLife.Generation -> List Svg.Svg
toSvg g = 
    let
        pts = pointifyAll g
        svg x = rect x [cellClass x]
        --svg x = polygon 8 x [Svg.Attributes.class (String.append "cell" <| Point.string [x])]
    in 
        List.map svg pts

cellClass : Point.Point -> Svg.Attribute
cellClass x = 
    let
        name = String.append "cell" <| Point.string [x]
    in
        Svg.Attributes.class name


viewBox g = 
    let
        box = enlarge 4.2 <| List.map toFloat <| withDefault [0, 0, 10, 10] (boundingBox g) 
        boxString = String.join " " <| List.map toString box
    in
        Svg.Attributes.viewBox boxString


boundingBox : GameOfLife.Generation -> Maybe (List Int)
boundingBox g =
    let
        cells = GameOfLife.toCells g
    in
        List.minimum cells `andThen` \(x,y) ->
        List.maximum cells `andThen` \(x2,y2) ->
        Nothing --Just [x, y, x2 - x, y2 - y]


enlarge : Float -> List Float -> List Float
enlarge k [x,y,w,h] =
    let
        diff = k - 1
    in
        [x - diff * w / 2, y - diff * h / 2, w * diff, h * diff]

pointify : GameOfLife.Cell -> Point.Point
pointify (x,y) = (toFloat x, toFloat y)


pointifyAll : GameOfLife.Generation -> List Point.Point
pointifyAll = List.map pointify << GameOfLife.toCells
