module VerifyExamples.Generator.Repeat0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generator exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#repeat: \n\n    repeat 1\n    |> advance 5\n    |> Tuple.first\n    --> [1, 1, 1, 1, 1]" <|
        \() ->
            Expect.equal
                (
                repeat 1
                |> advance 5
                |> Tuple.first
                )
                (
                [1, 1, 1, 1, 1]
                )