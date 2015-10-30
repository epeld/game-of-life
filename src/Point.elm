module Point where
import String exposing (join)

type alias Point = (Float, Float)
type alias Radians = Float


string : List Point -> String
string pts = join " " <| List.map show pts


-- Split a full turn (2pi) into n parts
splitTurn : Int -> List Radians
splitTurn n = 
    List.map (\x -> 2 * pi / toFloat n * toFloat x) [1..n]


-- Split a full turn (2pi) into (n+1) parts (where the last and first are the same!)
fullCircleRadians : Int -> List Radians
fullCircleRadians n = 
    let 
        rads = splitTurn n 
    in
        List.append rads (List.take 1 rads)
    


unitPolygon : Int -> List Point
unitPolygon n = List.map unit (fullCircleRadians n)


polygonPoints : Int -> Point -> Float -> List Point
polygonPoints n pt r = 
    let 
        poly = unitPolygon n
    in
        addAll pt (scaleAll r poly)


show : Point -> String
show (x,y) = "," `join` [toString x, toString y]


unit : Radians -> Point
unit r = (sin r, cos r)


scale : Float -> Point -> Point
scale k (x,y) = (k * x, k * y)


scaleAll : Float -> List Point -> List Point
scaleAll r pts = List.map (scale r) pts


add : Point -> Point -> Point
add (x,y) (a,b) = (x + a, y + b)


addAll : Point -> List Point -> List Point
addAll pt pts = List.map (add pt) pts

subtract : Point -> Point -> Point
subtract pt1 pt2 = add pt1 (negate pt2)


negate : Point -> Point
negate (x,y) = (-x,-y)
