module TestFibonacci exposing (..)

import Examples.Fibonacci as F
import Expect exposing (Expectation)
import Generator as G
import Test exposing (..)



--------------------------------------------------------------------------------


testFibonacci : Test
testFibonacci =
    describe "Test fibonacci series"
        [ test
            "fib1 == fib2"
            (\_ ->
                Tuple.first (G.advance 1000 F.fib1)
                    |> Expect.equal (Tuple.first (G.advance 1000 F.fib2))
            )
        ]
