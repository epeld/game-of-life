module GameOfLifeSvg where

import String
import Svg exposing (g, text, text')
import Svg.Lazy exposing (lazy2)
import Svg.Attributes exposing (fill, class, fontSize, x, y, textAnchor)
import Svg.Events exposing (onClick, onMouseOver)
import Html
import Html.Events exposing (onKeyPress, onDoubleClick)
import Signal exposing (Address)

import Point
import GameOfLife exposing (ClipBox, Generation, Cell, Update(..))
import Polygon exposing (rect)
import Box
import Time exposing (every, second)


type Action = Highlight Cell | Toggle Cell | ToggleRunning

type GUIUpdate = GUI Action | Logic Update

type alias GUIState = { generation : Generation, highlight : Maybe Cell, running : Bool }

timer = Signal.map (always (Logic Next)) (every second)

signal : ClipBox -> GUIState -> Signal Svg.Svg
signal b s = 
    let 
        mailbox = Signal.mailbox (GUI <| Highlight (0,0))
        updates = Signal.merge timer mailbox.signal
        gui = Signal.foldp (update b) s updates
        addr = Signal.forwardTo mailbox.address GUI
    in
       Signal.map (toScene addr b) gui


update : ClipBox -> GUIUpdate -> GUIState -> GUIState
update b u s =
    case u of
        GUI a -> 
            action b a s

        Logic l -> 
            if s.running
               then { s | generation <- GameOfLife.update (Just b) l s.generation }
               else s


action : ClipBox -> Action -> GUIState -> GUIState
action b a s =
    let
        logic x = { s | generation <- GameOfLife.update (Just b) x s.generation }
    in
        case a of
            Toggle c -> 
                if GameOfLife.contains c s.generation 
                then logic (Remove c)
                else logic (Add c)

            ToggleRunning ->
                { s | running <-  not s.running }

            Highlight c -> 
                { s | highlight <- Just c }


toScene : Address Action -> ClipBox -> GUIState -> Html.Html
toScene a b s = toSvgScene a b s


toSvgScene : Address Action -> ClipBox -> GUIState -> Svg.Svg
toSvgScene a b {generation, highlight, running} =
    let
        boxSvg = lazy2 boxToSvg a b
        genSvg = lazy2 generationToSvg a generation
        highlightSvg = lazy2 highlightToSvg a highlight
    in
        Svg.svg 
            [viewBox b, class "conways-game-of-life", onDoubleClick a ToggleRunning]
            [boxSvg, genSvg, highlightSvg, runningIndicator b running]


runningIndicator : ClipBox -> Bool -> Svg.Svg
runningIndicator b running = 
    let
        msg = if running 
                 then "Running. (Double-click to pause)"
                 else "Paused. Double-click to run!"

        size = if running then 1.0 else 5.0
        color = if running then "black" else "red"

        atts = [fontSize (toString size), fill color]

        common = [ x <| toString (Box.midx b)
                 , y <| toString (Box.boxy b + round size)
                 , textAnchor "middle"]

    in
       text' (List.append common atts) [text msg]


generationToSvg : Address Action -> Generation -> Svg.Svg
generationToSvg a gen = 
    let
        cells = GameOfLife.toCells gen

        highlight x = Highlight x |> Signal.message a

        atts x = [ fill "black" , onMouseOver (highlight x)]
    in
       g [class "generation"] <| List.map (\x -> rect (pointify x) (atts x)) cells


boxToSvg : Address Action -> ClipBox -> Svg.Svg
boxToSvg a b = 
    let
        pts = Box.points b 
        
        highlight x = Signal.message a (Highlight x)

        atts x = [ fill "white" , onMouseOver (highlight x)]
    in
       g [class "box"] <| List.map (\x -> rect (pointify x) (atts x)) pts


highlightToSvg : Address Action -> Maybe Cell -> Svg.Svg
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
                   [rect pt [fill "orange", onClick (toggle h')]]
    in
        g [class "highlight"] <| svg



viewBox : Box.Box number -> Svg.Attribute
viewBox box = Svg.Attributes.viewBox (Box.string box)


pointify : Cell -> Point.Point
pointify (x,y) = (toFloat x, toFloat y)


pointifyAll : Generation -> List Point.Point
pointifyAll = List.map pointify << GameOfLife.toCells
