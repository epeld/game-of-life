module GameOfLifeSvg where

import String
import Svg exposing (g)
import Svg.Lazy exposing (lazy2)
import Svg.Attributes exposing (fill, class)
import Svg.Events exposing (onClick, onMouseOver)
import Signal exposing (Address)

import Point
import GameOfLife exposing (ClipBox, Generation, Cell, Update(..))
import Polygon exposing (rect)
import Box


type GUIUpdate = Highlight Cell | Toggle Cell | Logic Update

type alias GUIState = { generation : Generation, highlight : Maybe Cell }


signal : ClipBox -> GUIState -> Signal Svg.Svg
signal b s = 
    let 
        mailbox = Signal.mailbox (Highlight (0,0))
        gui = Signal.foldp (update b) s mailbox.signal
    in
       Signal.map (toSvgScene mailbox.address b) gui


update : ClipBox -> GUIUpdate -> GUIState -> GUIState
update b u s =
    let
        b' = Just b
        logic x = { s | generation <- GameOfLife.update b' x s.generation }
    in
        case u of
            Toggle c -> 
                if GameOfLife.contains c s.generation 
                then logic (Remove c)
                else logic (Add c)

            Logic u' -> logic u'

            Highlight c -> 
                { s | highlight <- Just c }


toSvgScene : Address GUIUpdate -> ClipBox -> GUIState -> Svg.Svg
toSvgScene a b {generation, highlight} = 
    let
        boxSvg = lazy2 boxToSvg a b
        genSvg = lazy2 generationToSvg a generation
        highlightSvg = lazy2 highlightToSvg a highlight
    in
       Svg.svg [viewBox b] [boxSvg, genSvg, highlightSvg]


generationToSvg : Address GUIUpdate -> Generation -> Svg.Svg
generationToSvg a gen = 
    let
        cells = GameOfLife.toCells gen

        highlight x = Highlight x |> Signal.message a

        atts x = [ fill "black" , onMouseOver (highlight x)]
    in
       g [class "generation"] <| List.map (\x -> rect (pointify x) (atts x)) cells


boxToSvg : Address GUIUpdate -> ClipBox -> Svg.Svg
boxToSvg a b = 
    let
        pts = Box.points b 
        
        highlight x = Signal.message a (Highlight x)

        atts x = [ fill "white" , onMouseOver (highlight x)]
    in
       g [class "box"] <| List.map (\x -> rect (pointify x) (atts x)) pts


highlightToSvg : Address GUIUpdate -> Maybe Cell -> Svg.Svg
highlightToSvg a h =
    let
        mpt = Maybe.map pointify h
        toggle x = Toggle x |> Signal.message a 

        svg = case h of
            Nothing -> []
            Just h' -> 
                let 
                    pt = pointify h' 
                in 
                   [rect pt [fill "red", onClick (toggle h')]]
    in
        g [class "highlight"] <| svg



viewBox : Box.Box number -> Svg.Attribute
viewBox box = Svg.Attributes.viewBox (Box.string box)


pointify : Cell -> Point.Point
pointify (x,y) = (toFloat x, toFloat y)


pointifyAll : Generation -> List Point.Point
pointifyAll = List.map pointify << GameOfLife.toCells
