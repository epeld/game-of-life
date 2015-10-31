module Polygon where
import Point exposing (..)

import Svg 
import Svg.Attributes

distance = 1

circle : Point -> List Svg.Attribute -> Svg.Svg
circle (x,y) attrs = 
    let
        atts = [ Svg.Attributes.cx x'
               , Svg.Attributes.cy y'
               , Svg.Attributes.radius r' ]
        x' = toString x
        y' = toString y
        r' = toString (distance / 2)
    in 
        Svg.circle (List.append atts attrs) []


rect : Point -> List Svg.Attribute -> Svg.Svg
rect (x,y) attrs = 
    let
        atts = [ Svg.Attributes.x x'
               , Svg.Attributes.y y'
               , Svg.Attributes.width d'
               , Svg.Attributes.height d']
        x' = toString (x - distance / 2)
        y' = toString (y - distance / 2)
        d' = toString distance
    in 
        Svg.rect (List.append atts attrs) []


polygon : Int -> Point -> List Svg.Attribute -> Svg.Svg
polygon n pt attrs = 
    let
        radius = distance / cos (pi / toFloat n ) / 2
        points = pointsAttr (polygonPoints n pt radius)
        atts = List.append [points] attrs
    in
        Svg.polygon atts []


pointsAttr : List Point -> Svg.Attribute
pointsAttr pts = Svg.Attributes.points (string pts)


