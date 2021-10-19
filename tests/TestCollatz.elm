module TestCollatz exposing (..)

import Collatz exposing (..)
import Expect exposing (Expectation)
import Generator as G
import List.Extra as LE
import Test exposing (..)



--------------------------------------------------------------------------------


testCollatz : Test
testCollatz =
    describe
        "Test collatz generator assuming conjecture is true"
        [ test
            "test with some nummbers"
            (\_ ->
                [ 1, 2, 3, 5, 10, 100, 231, 1049 ]
                    |> List.map collatz
                    |> List.map G.toList
                    |> List.map LE.last
                    |> LE.unique
                    |> Expect.equal [ Just 1 ]
            )
        ]
