module VerifyExamples.Examples.Collatz.Collatz0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Examples.Collatz exposing (..)
import Generator as G







spec0 : Test.Test
spec0 =
    Test.test "#collatz: \n\n    collatz 10 |> G.toList\n    --> [10,5,16,8,4,2,1]" <|
        \() ->
            Expect.equal
                (
                collatz 10 |> G.toList
                )
                (
                [10,5,16,8,4,2,1]
                )