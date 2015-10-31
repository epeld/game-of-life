module Octagon where
import Point exposing (..)

import Svg 
import Svg.Attributes

radius = 0.5

circle : Point -> List Svg.Attribute -> Svg.Svg
circle (x,y) attrs = 
    let
        atts = [ Svg.Attributes.cx x'
               , Svg.Attributes.cy y'
               , Svg.Attributes.radius r' ]
        x' = toString x
        y' = toString y
        r' = toString radius
    in 
        Svg.circle (List.append atts attrs) []


octagon : Point -> List Svg.Attribute -> Svg.Svg
octagon pt attrs = 
    let
        points = pointsAttr (polygonPoints 8 pt radius)
        atts = List.append [points] attrs
    in
        Svg.polygon atts []


pointsAttr : List Point -> Svg.Attribute
pointsAttr pts = Svg.Attributes.points (string pts)


