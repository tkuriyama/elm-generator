module TestFinance exposing (..)

import Expect exposing (Expectation)
import Finance exposing (..)
import Generator as G
import List.Extra as LE
import Test exposing (..)



--------------------------------------------------------------------------------


approxEqual : Float -> Float -> Float -> Bool
approxEqual epsilon f1 f2 =
    abs (f1 - f2) <= epsilon


testFinance : Test
testFinance =
    describe
        "Test finance examples"
        [ test
            "test compounding with a few rates and rule of 72"
            (\_ ->
                [ ( 2, 1.02 ), ( 5, 1.05 ), ( 6, 1.06 ), ( 8, 1.08 ), ( 12, 1.12 ) ]
                    |> List.map (Tuple.mapSecond compound)
                    |> List.map (\( r, g ) -> G.take (round <| 72 / r) g)
                    |> List.map LE.last
                    |> List.map (Maybe.map (approxEqual 0.05 2.0))
                    |> LE.unique
                    |> Expect.equal [ Just True ]
            )
        , test
            "Test DCF with inverse cash flows"
            (\_ ->
                G.iterate ((*) 1.03) 1
                    |> G.tail
                    |> dcf 1.03
                    |> G.take 10
                    |> List.foldl (+) 0
                    |> Expect.equal 10
            )
        ]
