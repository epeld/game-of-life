module Octagon where
import Point exposing (..)

import Svg 
import Svg.Attributes

radius = 0.1


octagon : Point -> List Svg.Attribute -> Svg.Svg
octagon pt attrs = 
    let
        points = pointsAttr (polygonPoints 8 pt radius)
        atts = List.append [points] attrs
    in
        Svg.polygon atts []


pointsAttr : List Point -> Svg.Attribute
pointsAttr pts = Svg.Attributes.points (string pts)


