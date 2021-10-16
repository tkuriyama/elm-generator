module TestUtils exposing (..)

import Expect exposing (Expectation)
import Generator as G
import Internal.Types exposing (..)
import Internal.Utils exposing (..)
import Test exposing (..)



--------------------------------------------------------------------------------


testBind : Test
testBind =
    let
        g1 =
            G.repeat 1

        gEmpty =
            Empty

        f =
            \g -> Active { state = g.state, next = next_ g.next }

        next_ applyNext state =
            applyNext state
                |> Maybe.map
                    (\( value, state_ ) ->
                        ( Maybe.map ((+) 1) value, state_ )
                    )
    in
    describe "Test bind"
        [ test
            "empty"
            (\_ ->
                bind f gEmpty
                    |> Expect.equal Empty
            )
        , test
            "not empty"
            (\_ ->
                bind f g1
                    |> G.take 3
                    |> Expect.equal [ 2, 2, 2 ]
            )
        ]


testWithDefault : Test
testWithDefault =
    let
        g1 =
            G.iterate ((+) 1) 1
    in
    describe
        "Test withDefault"
        [ test
            "empty"
            (\_ ->
                withDefault 0 (\g -> 1) Empty
                    |> Expect.equal 0
            )
        , test
            "not empty"
            (\_ ->
                withDefault 0 (\g -> 1) g1
                    |> Expect.equal 1
            )
        ]


testGetNextValue : Test
testGetNextValue =
    let
        g1 =
            G.iterate ((+) 1) 1

        g2 =
            G.iterate ((+) 1) 1 |> G.filter ((<) 10)
    in
    describe "Test getNExtValue"
        [ test
            "unfiltered"
            (\_ ->
                g1
                    |> withDefault Nothing (\g -> getNextValue g.next g.state)
                    |> Expect.equal (Just ( Just 1, 2 ))
            )
        , test
            "filtered"
            (\_ ->
                g2
                    |> withDefault Nothing (\g -> getNextValue g.next g.state)
                    |> Expect.equal (Just ( Just 11, 12 ))
            )
        ]
