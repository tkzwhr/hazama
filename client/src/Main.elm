port module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E



-- https://commons.wikimedia.org/wiki/Category:Go_(set_of_square_images)?uselang=ja


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- Constants


constants =
    { cellSize = 50 }



-- Types


type StoneColor
    = Black
    | White
    | None


type Msg
    = ChangeBoardSize Int
    | ChangeLevel Int
    | ChangeOwnColor String
    | Start
    | Play String
    | Pass
    | Resign
    | ReceiveFromServer PortRecvMsg


type PortSendMsg
    = PSMStart Int Int StoneColor
    | PSMPlay StoneColor String
    | PSMEnd


type PortRecvMsg
    = PRMMove
    | PRMPass
    | PRMUpdateBlackStones (List String)
    | PRMUpdateWhiteStones (List String)
    | PRMUpdateCapturedBlackStones Int
    | PRMUpdateCapturedWhiteStones Int
    | PRMEnd String
    | PRMError



-- Type Aliases


type alias Model =
    { isPlaying : Bool
    , isPassed : Bool
    , score : Maybe String
    , boardSize : Int
    , level : Int
    , ownColor : StoneColor
    , numberOfCapturedBlackStones : Int
    , numberOfCapturedWhiteStones : Int
    , putBlackStones : List String
    , putWhiteStones : List String
    }


type alias Matrix a =
    List (List a)



-- Ports


port sendMessage : E.Value -> Cmd msg


port messageReceiver : (D.Value -> msg) -> Sub msg



-- Models


init : () -> ( Model, Cmd Msg )
init _ =
    ( { isPlaying = False
      , isPassed = False
      , score = Nothing
      , boardSize = 9
      , level = 5
      , ownColor = Black
      , numberOfCapturedBlackStones = 0
      , numberOfCapturedWhiteStones = 0
      , putBlackStones = []
      , putWhiteStones = []
      }
    , Cmd.none
    )



-- Updates


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeBoardSize boardSize ->
            ( { model | boardSize = boardSize }, Cmd.none )

        ChangeLevel level ->
            ( { model | level = level }, Cmd.none )

        ChangeOwnColor ownColor ->
            ( { model
                | ownColor =
                    if ownColor == "Black" then
                        Black

                    else
                        White
              }
            , Cmd.none
            )

        Start ->
            ( { model | isPlaying = True, score = Nothing, putBlackStones = [], putWhiteStones = [] }, PSMStart model.boardSize model.level model.ownColor |> encodePortSendMsg |> sendMessage )

        Play vertex ->
            ( { model | isPassed = False }, PSMPlay model.ownColor vertex |> encodePortSendMsg |> sendMessage )

        Pass ->
            if model.isPassed then
                ( { model | isPlaying = False, isPassed = False }, PSMEnd |> encodePortSendMsg |> sendMessage )

            else
                ( { model | isPassed = True }, PSMPlay model.ownColor "PASS" |> encodePortSendMsg |> sendMessage )

        Resign ->
            ( { model
                | isPlaying = False
                , score =
                    Just
                        (if model.ownColor == Black then
                            "W+R"

                         else
                            "B+R"
                        )
              }
            , Cmd.none
            )

        ReceiveFromServer recvMsg ->
            case recvMsg of
                PRMMove ->
                    ( { model | isPassed = False }, Cmd.none )

                PRMPass ->
                    if model.isPassed then
                        ( { model | isPlaying = False, isPassed = False }, PSMEnd |> encodePortSendMsg |> sendMessage )

                    else
                        ( { model | isPassed = True }, Cmd.none )

                PRMUpdateBlackStones vertices ->
                    ( { model | putBlackStones = vertices }, Cmd.none )

                PRMUpdateWhiteStones vertices ->
                    ( { model | putWhiteStones = vertices }, Cmd.none )

                PRMUpdateCapturedBlackStones num ->
                    ( { model | numberOfCapturedBlackStones = num }, Cmd.none )

                PRMUpdateCapturedWhiteStones num ->
                    ( { model | numberOfCapturedWhiteStones = num }, Cmd.none )

                PRMEnd score ->
                    ( { model | score = Just score }, Cmd.none )

                PRMError ->
                    ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver (\v -> v |> D.decodeValue decodePortRecvMsg |> Result.withDefault PRMError |> ReceiveFromServer)



-- Views


view : Model -> Html Msg
view model =
    div [ style "display" "flex", style "gap" "20px" ]
        [ div []
            [ goBoardView model.boardSize model.isPlaying model.putBlackStones model.putWhiteStones
            , numberOfCapturedStonesView model.numberOfCapturedBlackStones model.numberOfCapturedWhiteStones
            ]
        , div [ style "display" "flex", style "flex-direction" "column", style "gap" "20px" ]
            [ radioGroupView "boardSize"
                "Board Size"
                model.isPlaying
                (model.boardSize |> String.fromInt)
                (\v -> Maybe.withDefault 9 (String.toInt v) |> ChangeBoardSize)
                [ "6", "9", "13", "19" ]
            , radioGroupView
                "level"
                "Level"
                model.isPlaying
                (model.level |> String.fromInt)
                (\v -> Maybe.withDefault 5 (String.toInt v) |> ChangeLevel)
                [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10" ]
            , radioGroupView
                "stoneColor"
                "Stone Color"
                model.isPlaying
                (if model.ownColor == Black then
                    "Black"

                 else
                    "White"
                )
                ChangeOwnColor
                [ "Black", "White" ]
            , button [ type_ "button", disabled model.isPlaying, onClick Start ] [ text "Start" ]
            , button [ type_ "button", disabled (not model.isPlaying), onClick Pass ] [ text "Pass" ]
            , button [ type_ "button", disabled (not model.isPlaying), onClick Resign ] [ text "Resign" ]
            , div
                [ style "visibility"
                    (if model.isPassed then
                        "visible"

                     else
                        "hidden"
                    )
                , style "color" "red"
                ]
                [ text "PASSED" ]
            , div
                [ style "visibility"
                    (case model.score of
                        Just _ ->
                            "visible"

                        Nothing ->
                            "hidden"
                    )
                , style "color" "blue"
                ]
                [ text (Maybe.withDefault "" model.score)
                ]
            ]
        ]


goBoardView : Int -> Bool -> List String -> List String -> Html Msg
goBoardView boardSize isPlaying putBlackStones putWhiteStones =
    div
        [ style "display" "grid"
        , style "grid-template-columns" ("repeat(" ++ (boardSize |> String.fromInt) ++ ", " ++ (constants.cellSize |> String.fromInt) ++ "px)")
        , style "grid-auto-rows" ((constants.cellSize |> String.fromInt) ++ "px")
        ]
        (genInitialGoBoard boardSize
            |> updateGoBoard putBlackStones Black
            |> updateGoBoard putWhiteStones White
            |> goBoardRowViews boardSize isPlaying
            |> List.concat
        )


goBoardRowViews : Int -> Bool -> Matrix StoneColor -> Matrix (Html Msg)
goBoardRowViews boardSize isPlaying boardRowsData =
    boardRowsData
        |> List.indexedMap
            (\i br ->
                if i == 0 then
                    goBoardColumnViews boardSize isPlaying br i "u"

                else if i == boardSize - 1 then
                    goBoardColumnViews boardSize isPlaying br i "d"

                else
                    goBoardColumnViews boardSize isPlaying br i "c"
            )


goBoardColumnViews : Int -> Bool -> List StoneColor -> Int -> String -> List (Html Msg)
goBoardColumnViews boardSize isPlaying boardRowData row colType =
    boardRowData
        |> List.indexedMap
            (\j bc ->
                if bc == Black then
                    cellView boardSize isPlaying row j "b"

                else if bc == White then
                    cellView boardSize isPlaying row j "w"

                else if j == 0 then
                    cellView boardSize isPlaying row j (colType ++ "l")

                else if j == boardSize - 1 then
                    cellView boardSize isPlaying row j (colType ++ "r")

                else
                    cellView boardSize isPlaying row j (colType ++ "c")
            )


cellView : Int -> Bool -> Int -> Int -> String -> Html Msg
cellView boardSize isPlaying row column cellType =
    if cellType == "b" || cellType == "w" || not isPlaying then
        img [ src ("assets/images/Go_" ++ cellType ++ ".svg"), width constants.cellSize ] []

    else
        a [ href "#", onClick (Play (( boardSize - row - 1, column ) |> pointToVertex)) ]
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


radioGroupView : String -> String -> Bool -> String -> (String -> Msg) -> List String -> Html Msg
radioGroupView groupName groupLabel disabled_ selected onSelect values =
    div []
        [ div [ style "font-weight" "bold" ] [ text groupLabel ]
        , div []
            (values
                |> List.map
                    (\value_ ->
                        label []
                            [ input
                                [ type_ "radio"
                                , name groupName
                                , value value_
                                , disabled disabled_
                                , checked (value_ == selected)
                                , onInput (\v -> onSelect v)
                                ]
                                []
                            , text value_
                            ]
                    )
            )
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
    -- I(65 + 8)は使われていないためスキップする
    (Char.fromCode
        (65
            + row
            + (if row >= 8 then
                1

               else
                0
              )
        )
        |> String.fromChar
    )
        ++ String.fromInt (column + 1)



-- Encoder/Decoder


encodePortSendMsg : PortSendMsg -> E.Value
encodePortSendMsg portSendMsg =
    case portSendMsg of
        PSMStart boardSize level ownColor ->
            E.object
                [ ( "command", E.string "start" )
                , ( "boardSize", E.int boardSize )
                , ( "level", E.int level )
                , ( "ownColor"
                  , E.string
                        (if ownColor == Black then
                            "black"

                         else
                            "white"
                        )
                  )
                ]

        PSMPlay ownColor vertex ->
            E.object
                [ ( "command", E.string "play" )
                , ( "ownColor"
                  , E.string
                        (if ownColor == Black then
                            "black"

                         else
                            "white"
                        )
                  )
                , ( "vertex", E.string vertex )
                ]

        PSMEnd ->
            E.object [ ( "command", E.string "end" ) ]


decodePortRecvMsg : D.Decoder PortRecvMsg
decodePortRecvMsg =
    D.field "command" D.string
        |> D.andThen
            (\command ->
                case command of
                    "move" ->
                        D.succeed PRMMove

                    "pass" ->
                        D.succeed PRMPass

                    "updateBlackStones" ->
                        D.map PRMUpdateBlackStones (D.field "vertices" (D.list D.string))

                    "updateWhiteStones" ->
                        D.map PRMUpdateWhiteStones (D.field "vertices" (D.list D.string))

                    "updateCapturedBlackStones" ->
                        D.map PRMUpdateCapturedBlackStones (D.field "number" D.int)

                    "updateCapturedWhiteStones" ->
                        D.map PRMUpdateCapturedWhiteStones (D.field "number" D.int)

                    "end" ->
                        D.map PRMEnd (D.field "score" D.string)

                    _ ->
                        D.succeed PRMError
            )
