module Main exposing (main)

import Browser
import Html exposing (Html, div, img)
import Html.Attributes exposing (..)



-- https://commons.wikimedia.org/wiki/Category:Go_(set_of_square_images)?uselang=ja


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Constants


constants =
    { cellSize = 50 }



-- Type Aliases


type alias Model =
    { boardSize : Int }



-- Models


init : Model
init =
    { boardSize = 9 }



-- Updates


update : msg -> Model -> Model
update _ _ =
    init



-- Views


view : Model -> Html msg
view model =
    goBoardView model.boardSize


goBoardView : Int -> Html msg
goBoardView boardSize =
    div
        [ style "display" "grid"
        , style "grid-template-columns" ("repeat(" ++ (boardSize |> String.fromInt) ++ ", " ++ (constants.cellSize |> String.fromInt) ++ "px)")
        , style "grid-auto-rows" ((constants.cellSize |> String.fromInt) ++ "px")
        ]
        (goBoardRowViews boardSize |> List.concat)


goBoardRowViews : Int -> List (List (Html msg))
goBoardRowViews boardSize =
    List.range 0 (boardSize - 1)
        |> List.map
            (\i ->
                if i == 0 then
                    goBoardColumnViews boardSize "u"

                else if i == boardSize - 1 then
                    goBoardColumnViews boardSize "d"

                else
                    goBoardColumnViews boardSize "c"
            )


goBoardColumnViews : Int -> String -> List (Html msg)
goBoardColumnViews boardSize colType =
    List.range 0 (boardSize - 1)
        |> List.map
            (\i ->
                if i == 0 then
                    cellView (colType ++ "l")

                else if i == boardSize - 1 then
                    cellView (colType ++ "r")

                else
                    cellView (colType ++ "c")
            )


cellView : String -> Html msg
cellView cellType =
    img [ src ("assets/images/Go_" ++ cellType ++ ".svg"), width constants.cellSize ] []
