module Main exposing (..)

import Html exposing (Html, div, text, table, tr, td)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Nonempty exposing (..)


type alias Model =
    { turn : Mark
    , board : Nonempty (Nonempty Cell)
    , winner : Maybe Mark
    }


type Mark
    = X
    | O


type Cell
    = E
    | Marked Mark


type Msg
    = Place ( Int, Int )
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Place pos ->
            ( place model pos |> checkWin |> nextPlayersTurn, Cmd.none )

        Reset ->
            init


checkWin : Model -> Model
checkWin model =
    let
        won =
            foldl (\a b -> b || rowWin a) False model.board

        winner =
            if won then
                Just model.turn
            else
                Nothing
    in
        { model | winner = winner }


rowWin : Nonempty Cell -> Bool
rowWin row =
    let
        first =
            head row
    in
        row
            |> filter ((==) first) E
            |> length
            |> (==) 3
            |> (&&) (first /= E)


place : Model -> ( Int, Int ) -> Model
place model pos =
    { model | board = boardInsert pos model.turn model.board }


nextPlayersTurn : Model -> Model
nextPlayersTurn model =
    case model.turn of
        X ->
            { model | turn = O }

        O ->
            { model | turn = X }


boardInsert : ( Int, Int ) -> Mark -> Nonempty (Nonempty Cell) -> Nonempty (Nonempty Cell)
boardInsert ( x, y ) player board =
    let
        help newVal pred i val =
            if pred i then
                Marked newVal
            else
                val

        updateColumn =
            help player (\n -> n == x)

        updateRow i row =
            if i == y then
                indexedMap updateColumn row
            else
                row
    in
        indexedMap updateRow board


view : Model -> Html Msg
view model =
    let
        wintext =
            case model.winner of
                Just a ->
                    case a of
                        X ->
                            "X won!"

                        O ->
                            "O won!"

                Nothing ->
                    "Game is on"

        attributes =
            case model.winner of
                Nothing ->
                    []

                Just _ ->
                    [ onClick Reset ]
    in
        div attributes
            [ text wintext
            , board model
            ]


board : Model -> Html Msg
board { turn, board } =
    indexedMap drawRow board
        |> toList
        |> table
            [ style
                [ ( "border", "2px solid white" )
                , ( "border-collapse", "collapse" )
                , ( "width", "300px" )
                , ( "height", "300px" )
                ]
            ]


drawRow : Int -> Nonempty Cell -> Html Msg
drawRow rowIndex row =
    indexedMap (drawCell rowIndex) row |> toList |> tr []


drawCell : Int -> Int -> Cell -> Html Msg
drawCell y x value =
    let
        mark =
            [ div
                [ style
                    [ ( "display", "flex" )
                    , ( "justify-content", "center" )
                    , ( "align-items", "center" )
                    ]
                ]
                (case value of
                    E ->
                        []

                    Marked m ->
                        case m of
                            X ->
                                [ text "X" ]

                            O ->
                                [ text "O" ]
                )
            ]
    in
        td
            [ style
                [ ( "border", "1px solid black" )
                , ( "width", "100px" )
                , ( "height", "100px" )
                ]
            , onClick (Place ( x, y ))
            ]
            mark


init : ( Model, Cmd Msg )
init =
    Model X emptyBoard Nothing ! []


emptyBoard : Nonempty (Nonempty Cell)
emptyBoard =
    listOfThree (listOfThree E)


listOfThree : a -> Nonempty a
listOfThree a =
    Nonempty a [ a, a ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
