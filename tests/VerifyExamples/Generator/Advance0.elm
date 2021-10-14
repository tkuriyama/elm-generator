module VerifyExamples.Generator.Advance0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generator exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#advance: \n\n    iterate ((+) 1) 1\n    |> advance 5       -- ([1, 2, 3, 4, 5], updated generator)\n    |> Tuple.second\n    |> advance 5        -- ([6, 7, 8, 9, 10], updated generator)\n    |> Tuple.first\n    --> [6, 7, 8, 9, 10]" <|
        \() ->
            Expect.equal
                (
                iterate ((+) 1) 1
                |> advance 5       -- ([1, 2, 3, 4, 5], updated generator)
                |> Tuple.second
                |> advance 5        -- ([6, 7, 8, 9, 10], updated generator)
                |> Tuple.first
                )
                (
                [6, 7, 8, 9, 10]
                )