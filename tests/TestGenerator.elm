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


testEmpty : Test
testEmpty =
    let
        emptyG =
            G.fromList [ 1, 2, 3 ] |> G.drop 3
    in
    describe "Test various empty scenarios"
        [ test
            "take from empty"
            (\_ ->
                G.take 5 emptyG
                    |> Expect.equal []
            )
        , test
            "takeWhile from empty"
            (\_ ->
                G.takeWhile ((<) 10) emptyG
                    |> Expect.equal []
            )
        , test
            "drop from empty"
            (\_ ->
                G.drop 5 emptyG
                    |> G.empty
                    |> Expect.equal True
            )
        , test
            "dropWhile from empty"
            (\_ ->
                G.dropWhile ((<) 10) emptyG
                    |> G.empty
                    |> Expect.equal True
            )
        , test
            "map over empty"
            (\_ ->
                G.map ((+) 1) emptyG
                    |> G.take 1
                    |> Expect.equal []
            )
        , test
            "filter over empty"
            (\_ ->
                G.filter (\_ -> True) emptyG
                    |> G.take 1
                    |> Expect.equal []
            )
        , test
            "scanl over empty"
            (\_ ->
                G.scanl (+) 0 emptyG
                    |> G.take 1
                    |> Expect.equal []
            )
        , test
            "scanl over empty |> head"
            (\_ ->
                G.scanl (+) 0 emptyG
                    |> G.head
                    |> Expect.equal Nothing
            )
        , test
            "scanl over empty |> tail"
            (\_ ->
                G.scanl (+) 0 emptyG
                    |> G.tail
                    |> G.empty
                    |> Expect.equal True
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



--------------------------------------------------------------------------------


testMerge : Test
testMerge =
    describe "Test merging works as expected"
        [ test
            "filter and merge together"
            (\_ ->
                G.iterate ((+) 1) 1
                    |> G.filter ((<) 3)
                    -- [11, 12...]
                    |> G.merge (<) (G.iterate ((+) 1) 1)
                    |> G.take 5
                    |> Expect.equal [ 1, 2, 3, 4, 4 ]
            )
        , test
            "merge empty generators -> empty"
            (\_ ->
                G.fromList []
                    |> (\g ->
                            G.merge (<) g g
                                |> G.empty
                                |> Expect.equal True
                       )
            )
        , test
            "merge filtered empty list -> empty"
            (\_ ->
                G.fromList [ 1, 2, 3 ]
                    |> G.filter ((<) 3)
                    |> (\g ->
                            G.merge (<) g g
                                |> G.take 3
                                |> Expect.equal []
                       )
            )
        ]
