port module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E


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


type alias Coord =
    ( Int, Int )


type alias Point =
    ( Coord, StoneColor )



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
    messageReceiver parseMessage


parseMessage : D.Value -> Msg
parseMessage decodable =
    decodable
        |> D.decodeValue decodePortRecvMsg
        |> Result.withDefault PRMError
        |> ReceiveFromServer



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
                [ "9", "13", "19" ]
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
        (gen2DPointList boardSize
            |> applyStonesTo2DCoordList putBlackStones Black
            |> applyStonesTo2DCoordList putWhiteStones White
            |> List.map (cellView boardSize isPlaying)
        )


cellView : Int -> Bool -> Point -> Html Msg
cellView boardSize isPlaying ( coord, stoneColor ) =
    let
        imgView piece =
            img [ src ("assets/images/Go_" ++ piece ++ ".svg"), width constants.cellSize ] []
    in
    case stoneColor of
        Black ->
            imgView "b"

        White ->
            imgView "w"

        None ->
            if isPlaying then
                a [ href "#", onClick (Play (coordToStr coord)) ] [ imgView (toGoBoardPiece boardSize coord) ]

            else
                imgView (toGoBoardPiece boardSize coord)


numberOfCapturedStonesView : Int -> Int -> Html Msg
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


gen2DPointList : Int -> List Point
gen2DPointList size =
    List.range 1 size
        |> List.map
            (\r ->
                List.range 1 size
                    |> List.map
                        (\c ->
                            ( ( r, c ), None )
                        )
            )
        |> List.reverse
        |> List.concat


toGoBoardPiece : Int -> Coord -> String
toGoBoardPiece size ( r, c ) =
    let
        rStr =
            if r == 1 then
                "d"

            else if r == size then
                "u"

            else
                "c"

        cStr =
            if c == 1 then
                "l"

            else if c == size then
                "r"

            else
                "c"
    in
    rStr ++ cStr


applyStonesTo2DCoordList : List String -> StoneColor -> List Point -> List Point
applyStonesTo2DCoordList putStones stoneColor original2DCoordList =
    original2DCoordList
        |> List.map
            (\( coord, originalStoneColor ) ->
                if List.any (\e -> e == coordToStr coord) putStones then
                    ( coord, stoneColor )

                else
                    ( coord, originalStoneColor )
            )


coordToStr : Coord -> String
coordToStr ( r, c ) =
    let
        -- I(64 + 9)は使われていないためスキップする
        shift =
            if r >= 9 then
                1

            else
                0

        rStr =
            Char.fromCode (64 + r + shift) |> String.fromChar

        cStr =
            String.fromInt c
    in
    rStr ++ cStr



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
