module Main exposing (..)

import Html exposing (Html, div, text, table, tr, td, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Nonempty exposing (..)


type alias Model =
    { turn : Mark
    , board : Nonempty (Nonempty Cell)
    , gameStatus : Status
    , history : List Msg
    }


type Status
    = GameOn
    | Draw
    | Winner Mark


type Mark
    = X
    | O


type Cell
    = E
    | Marked Mark


type Msg
    = Place ( Int, Int )
    | Reset
    | Undo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelper msg model, Cmd.none )


updateHelper : Msg -> Model -> Model
updateHelper msg model =
    case msg of
        Place pos ->
            if legalMove model.board pos then
                place model pos
                    |> recordMsg msg
                    |> checkWin
                    |> checkDraw
                    |> nextPlayersTurn
            else
                model

        Reset ->
            initModel

        Undo ->
            undoLast model


undoLast : Model -> Model
undoLast model =
    let
        tailOfHistory =
            case List.tail model.history of
                Just t ->
                    t

                Nothing ->
                    []
    in
        -- Coolest function in the hole app
        List.foldr updateHelper initModel tailOfHistory


recordMsg : Msg -> Model -> Model
recordMsg msg model =
    { model | history = msg :: model.history }


legalMove : Nonempty (Nonempty Cell) -> ( Int, Int ) -> Bool
legalMove board ( x, y ) =
    get y board |> get x |> (==) E


checkDraw : Model -> Model
checkDraw model =
    let
        noEmptyCellOnRow =
            all ((/=) E)

        draw =
            all noEmptyCellOnRow model.board
    in
        if draw then
            { model | gameStatus = Draw }
        else
            model


checkWin : Model -> Model
checkWin model =
    let
        won =
            rowWin model.board || columnWin model.board || checkDiagonalWin model.board

        newStatus =
            if won then
                Winner model.turn
            else
                GameOn
    in
        { model | gameStatus = newStatus }


checkDiagonalWin : Nonempty (Nonempty Cell) -> Bool
checkDiagonalWin board =
    let
        diagonal1 =
            Nonempty (get 0 board |> get 0)
                [ (get 1 board |> get 1)
                , (get 2 board |> get 2)
                ]

        diagonal2 =
            Nonempty (get 0 board |> get 2)
                [ (get 1 board |> get 1)
                , (get 2 board |> get 0)
                ]
    in
        (rowWinHelper diagonal1) || (rowWinHelper diagonal2)


columnWin : Nonempty (Nonempty Cell) -> Bool
columnWin board =
    let
        column : Int -> Nonempty Cell
        column n =
            map (\row -> get n row) board

        columnW =
            column >> rowWinHelper
    in
        List.map columnW [ 0, 1, 2 ]
            |> List.any identity


rowWin : Nonempty (Nonempty Cell) -> Bool
rowWin board =
    foldl (\a b -> b || rowWinHelper a) False board


rowWinHelper : Nonempty Cell -> Bool
rowWinHelper row =
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
boardInsert ( x, y ) mark board =
    let
        help newVal pred i val =
            if pred i then
                Marked newVal
            else
                val

        updateColumn =
            help mark (\n -> n == x)

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
            case model.gameStatus of
                Winner a ->
                    case a of
                        X ->
                            "X won!"

                        O ->
                            "O won!"

                GameOn ->
                    "Game is on"

                Draw ->
                    "It's a draw"

        attributes =
            case model.gameStatus of
                GameOn ->
                    []

                _ ->
                    [ onClick Reset ]
    in
        div []
            [ div attributes
                [ text wintext
                , board model
                ]
            , button [ onClick Undo ] [ text "Undo" ]
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
    ( initModel, Cmd.none )


initModel : Model
initModel =
    Model X emptyBoard GameOn []


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
