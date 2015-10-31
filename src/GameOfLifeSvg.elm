module GameOfLifeSvg where

import Svg

import Point
import GameOfLife
import Octagon


toSvg : List Cell -> List Svg.Svg
toSvg cells = 
    let
        pts = pointifyAll cells
        oct x = octagon x attrs
    in 
        List.map oct pts attrs


pointify : GameOfLife.Cell -> Point.Point
pointify (x,y) = (toFloat x, toFloat y)


pointifyAll : List GameOfLife.Cell -> List Point.Point
pointifyAll = List.map pointify
