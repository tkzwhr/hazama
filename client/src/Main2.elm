module Main exposing (main)

import Browser
import Html exposing (Html, a, div, img, text)
import Html.Attributes exposing (..)



-- https://commons.wikimedia.org/wiki/Category:Go_(set_of_square_images)?uselang=ja


main =
    Browser.sandbox { init = init, update = update, view = view }



-- Constants


constants =
    { cellSize = 50 }



-- Types


type StoneColor
    = Black
    | White
    | None



-- Type Aliases


type alias Model =
    { boardSize : Int
    , numberOfCapturedBlackStones : Int
    , numberOfCapturedWhiteStones : Int
    , putBlackStones : List String
    , putWhiteStones : List String
    }


type alias Matrix a =
    List (List a)



-- Models


init : Model
init =
    { boardSize = 9
    , numberOfCapturedBlackStones = 0
    , numberOfCapturedWhiteStones = 0
    , putBlackStones = [ "A1", "B2" ]
    , putWhiteStones = [ "C1" ]
    }



-- Updates


update : msg -> Model -> Model
update _ _ =
    init



-- Views


view : Model -> Html msg
view model =
    div []
        [ goBoardView model.boardSize model.putBlackStones model.putWhiteStones
        , numberOfCapturedStonesView model.numberOfCapturedBlackStones model.numberOfCapturedWhiteStones
        ]


goBoardView : Int -> List String -> List String -> Html msg
goBoardView boardSize putBlackStones putWhiteStones =
    div
        [ style "display" "grid"
        , style "grid-template-columns" ("repeat(" ++ (boardSize |> String.fromInt) ++ ", " ++ (constants.cellSize |> String.fromInt) ++ "px)")
        , style "grid-auto-rows" ((constants.cellSize |> String.fromInt) ++ "px")
        ]
        (genInitialGoBoard boardSize
            |> updateGoBoard putBlackStones Black
            |> updateGoBoard putWhiteStones White
            |> goBoardRowViews boardSize
            |> List.concat
        )


goBoardRowViews : Int -> Matrix StoneColor -> Matrix (Html msg)
goBoardRowViews boardSize boardRowsData =
    boardRowsData
        |> List.indexedMap
            (\i br ->
                if i == 0 then
                    goBoardColumnViews boardSize br i "u"

                else if i == boardSize - 1 then
                    goBoardColumnViews boardSize br i "d"

                else
                    goBoardColumnViews boardSize br i "c"
            )


goBoardColumnViews : Int -> List StoneColor -> Int -> String -> List (Html msg)
goBoardColumnViews boardSize boardRowData row colType =
    boardRowData
        |> List.indexedMap
            (\j bc ->
                if bc == Black then
                    cellView boardSize row j "b"

                else if bc == White then
                    cellView boardSize row j "w"

                else if j == 0 then
                    cellView boardSize row j (colType ++ "l")

                else if j == boardSize - 1 then
                    cellView boardSize row j (colType ++ "r")

                else
                    cellView boardSize row j (colType ++ "c")
            )


cellView : Int -> Int -> Int -> String -> Html msg
cellView boardSize row column cellType =
    if cellType == "b" || cellType == "w" then
        img [ src ("assets/images/Go_" ++ cellType ++ ".svg"), width constants.cellSize ] []

    else
        a [ href ("#" ++ (( boardSize - row - 1, column ) |> pointToVertex)) ]
            [ img [ src ("assets/images/Go_" ++ cellType ++ ".svg"), width constants.cellSize ] []
            ]


numberOfCapturedStonesView : Int -> Int -> Html msg
numberOfCapturedStonesView black white =
    div
        [ style "display" "flex"
        , style "gap" "20px"
        , style "align-items" "center"
        , style "justify-content" "space-around"
        , style "margin-top" "20px"
        , style "padding" "10px"
        , style "background-color" "#DCB35C"
        , style "width" "250px"
        , style "font-size" "18pt"
        ]
        [ img [ src "assets/images/Go_b.svg", width constants.cellSize ] []
        , div [] [ text (black |> String.fromInt) ]
        , img [ src "assets/images/Go_w.svg", width constants.cellSize ] []
        , div [] [ text (white |> String.fromInt) ]
        ]



-- View Logics


genInitialGoBoard : Int -> Matrix StoneColor
genInitialGoBoard size =
    List.range 1 size
        |> List.map
            (\_ ->
                List.range 1 size
                    |> List.map
                        (\_ ->
                            None
                        )
            )


updateGoBoard : List String -> StoneColor -> Matrix StoneColor -> Matrix StoneColor
updateGoBoard putStones stoneColor originalBoard =
    originalBoard
        -- originalBoardの上下反転を解除する
        |> List.reverse
        |> List.indexedMap
            (\i br ->
                br
                    |> List.indexedMap
                        (\j bc ->
                            if putStones |> List.any (\ps -> pointToVertex ( i, j ) == ps) then
                                stoneColor

                            else
                                bc
                        )
            )
        -- GoBoardは左上が基準なのに対し、GNU Goは左下基準のため上下を反転させる
        |> List.reverse


pointToVertex : ( Int, Int ) -> String
pointToVertex ( row, column ) =
    (Char.fromCode (65 + row) |> String.fromChar) ++ String.fromInt (column + 1)
