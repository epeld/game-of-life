module Main where
import GameOfLifeSvg as GUI
import GameOfLife
import Box exposing (Box)

import Time
import Signal
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (text)


initial = GameOfLife.fromCells [(3,2), (1,1), (2,1), (2,2), (1,0), (0,2)]


bounds = Box.Box -5 -5 10 10


main = GUI.signal bounds { generation = initial, highlight = Nothing }
