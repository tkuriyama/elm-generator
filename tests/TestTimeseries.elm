module TestTimeseries exposing (..)

import Expect exposing (Expectation)
import Generator as G
import Test exposing (..)
import Timeseries as TS



--------------------------------------------------------------------------------


testTimeseries : Test
testTimeseries =
    let
        ts0 =
            G.fromList [ ( 1, Nothing ) ]

        ts1 =
            G.fromList [ ( 1, Just 1.0 ) ]

        ts2 =
            G.fromList [ ( 2, Just 1.1 ) ]

        ts3 =
            G.fromList [ ( 3, Just 1.2 ) ]
    in
    describe
        "Test timeseries merging"
        [ test
            "explicit Nothing"
            (\_ ->
                TS.toSet "zero" ts0
                    |> TS.alignToSet "one" ts1
                    |> (\tsSet -> G.take 2 tsSet.set)
                    |> Expect.equal
                        [ ( 1, [ Just 1.0, Nothing ] ) ]
            )
        , test
            "implicit Nothing"
            (\_ ->
                TS.toSet "one" ts1
                    |> TS.alignToSet "two" ts2
                    |> (\tsSet -> G.take 2 tsSet.set)
                    |> Expect.equal
                        [ ( 1, [ Nothing, Just 1.0 ] )
                        , ( 2, [ Just 1.1, Nothing ] )
                        ]
            )
        , test
            "implicit Nothing with multiple values"
            (\_ ->
                TS.toSet "one" ts1
                    |> TS.alignToSet "two" ts2
                    |> TS.alignToSet "three" ts3
                    |> (\tsSet -> G.take 5 tsSet.set)
                    |> Expect.equal
                        [ ( 1, [ Nothing, Nothing, Just 1.0 ] )
                        , ( 2, [ Nothing, Just 1.1, Nothing ] )
                        , ( 3, [ Just 1.2, Nothing, Nothing ] )
                        ]
            )
        ]
