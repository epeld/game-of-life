module Main where
import Octagon exposing (..)
import Html exposing (text)

import Svg exposing (..)
import Svg.Attributes exposing (..)

main = 
    svg
        [ width "10cm"
        , height "10cm"
        , viewBox "0 0 1 1" ]
        [octagon (0.5, 0.5) 
            [ fill "black" ]]
    
