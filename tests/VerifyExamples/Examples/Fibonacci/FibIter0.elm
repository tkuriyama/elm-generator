module VerifyExamples.Examples.Fibonacci.FibIter0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Examples.Fibonacci exposing (..)
import Generator as G







spec0 : Test.Test
spec0 =
    Test.test "#fibIter: \n\n    fib1\n    |> G.take 10\n    --> [0,1,1,2,3,5,8,13,21,34]" <|
        \() ->
            Expect.equal
                (
                fib1
                |> G.take 10
                )
                (
                [0,1,1,2,3,5,8,13,21,34]
                )