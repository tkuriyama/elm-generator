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

        emptyG =
            G.fromList [ 1 ] |> G.drop 1
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
        , test
            "cons to empty is same as prefix to empty"
            (\_ ->
                G.cons 1 emptyG
                    |> G.take 2
                    |> Expect.equal (G.prefix [ 1 ] emptyG |> G.take 2)
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
        , test
            "filter and scanl"
            (\_ ->
                G.iterate ((+) 1) 1
                    |> G.filter ((<) 10)
                    |> G.scanl (+) 0
                    |> G.take 3
                    |> Expect.equal [ 11, 23, 36 ]
            )
        ]



--------------------------------------------------------------------------------


testZip : Test
testZip =
    describe "Test zipping works as expected"
        [ test
            "filter and zip together"
            (\_ ->
                G.iterate ((+) 1) 1
                    |> G.filter ((<) 10)
                    -- [11, 12...]
                    |> G.zipWith (+) (G.iterate ((+) 1) 1)
                    |> G.take 5
                    |> Expect.equal [ 12, 14, 16, 18, 20 ]
            )
        , test
            "zip empty generators -> empty"
            (\_ ->
                G.fromList []
                    |> (\g ->
                            G.zip g g
                                |> G.empty
                                |> Expect.equal True
                       )
            )
        , test
            "zip filtered empty list -> empty"
            (\_ ->
                G.fromList [ 1, 2, 3 ]
                    |> (\g ->
                            G.zip g (G.filter ((<) 3) g)
                                |> G.take 3
                                |> Expect.equal []
                       )
            )
        , test
            "zip filtered non-empty list -> non-empty"
            (\_ ->
                G.fromList [ 1, 2, 3 ]
                    |> (\g ->
                            G.zip g (G.filter ((<) 1) g)
                                |> G.take 3
                                |> Expect.equal [ ( 1, 2 ), ( 2, 3 ) ]
                       )
            )
        ]
