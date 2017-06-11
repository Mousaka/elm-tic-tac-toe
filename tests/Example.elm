module Example exposing (..)

import Test exposing (..)
import Expect


-- import Fuzz exposing (list, int, string)

import Main exposing (place, emptyBoard, boardInsert, rowWin, checkWin, Cell(..), Mark(..))
import List.Nonempty exposing ((:::), fromElement, Nonempty(..))


suite : Test
suite =
    describe "Main module tests"
        [ describe "boardInsert"
            -- Nest as many descriptions as you like.
            [ test "Cross at first cell (upper left)" <|
                \_ ->
                    let
                        row1 =
                            Nonempty (Marked X) [ E, E ]

                        row2 =
                            Nonempty E [ E, E ]

                        row3 =
                            row2

                        expected =
                            Nonempty row1 [ row2, row3 ]
                    in
                        Expect.equal expected <| boardInsert ( 0, 0 ) X emptyBoard
            , test "Circle at middel cell" <|
                \_ ->
                    let
                        row1 =
                            Nonempty E [ E, E ]

                        row2 =
                            Nonempty E [ (Marked O), E ]

                        row3 =
                            row1

                        expected =
                            Nonempty row1 [ row2, row3 ]
                    in
                        Expect.equal expected <| boardInsert ( 1, 1 ) O emptyBoard
            ]
        , describe "Row win"
            [ test "Row is win for X" <|
                rowTest True (Nonempty (Marked X) [ (Marked X), (Marked X) ])
            , test "Row is win for O" <|
                rowTest True (Nonempty (Marked O) [ (Marked O), (Marked O) ])
            , test "Row is not win because empty" <|
                rowTest False (Nonempty E [ E, E ])
            , test "Row is not win because one is not like the other" <|
                rowTest False (Nonempty (Marked X) [ (Marked X), (Marked O) ])
            ]
        , describe "Check win"
            [ test "why no win" <|
                \_ ->
                    let
                        row1 =
                            Nonempty E [ E, E ]

                        row2 =
                            row1

                        row3 =
                            Nonempty (Marked O) [ (Marked O), (Marked O) ]

                        board =
                            Nonempty row1 [ row2, row3 ]

                        model =
                            Main.Model O board Nothing
                    in
                        Expect.equal (Just O) <| .winner <| checkWin model
            ]
        ]


rowTest : Bool -> Nonempty Cell -> a -> Expect.Expectation
rowTest expected row =
    \_ -> Expect.equal expected <| rowWin row
