module TestGenerator exposing (..)

import Expect exposing (Expectation)
import Generator as G
import Test exposing (..)



--------------------------------------------------------------------------------


testSetup : Test
testSetup =
    let
        ints =
            G.init 1 (\n -> Just ( n, n + 1 ))

        ints2 =
            G.iterate ((+) 1) 1

        ones =
            G.init 1 (\n -> Just ( n, n ))

        ones2 =
            G.repeat 1
    in
    describe "Test generator setup"
        [ test
            "init"
            (\_ ->
                Tuple.first (G.advance 1 ints)
                    |> Expect.equal [ 1 ]
            )
        , test
            "init and repeat equivalcen"
            (\_ ->
                Tuple.first (G.advance 100 ones)
                    |> Expect.equal (Tuple.first (G.advance 100 ones2))
            )
        , test
            "init and iterate equivalence"
            (\_ ->
                Tuple.first (G.advance 100 ints)
                    |> Expect.equal (Tuple.first (G.advance 100 ints2))
            )
        ]



--------------------------------------------------------------------------------


testTransforms : Test
testTransforms =
    describe "Test transformations"
        [ test
            "filter and map chain"
            (\_ ->
                G.iterate ((+) 1) 1
                    |> G.filter ((<) 10)
                    -- [11, 12...]
                    |> G.map ((+) 10)
                    -- [21, 22...]
                    |> G.map ((+) 1)
                    -- [22, 23...]
                    |> G.filter ((<) 23)
                    -- [24, 25...]
                    |> G.take 5
                    |> Expect.equal [ 24, 25, 26, 27, 28 ]
            )
        ]
