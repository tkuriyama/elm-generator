module VerifyExamples.Generator.TakeWhile0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generator exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#takeWhile: \n\n    iterate ((+) 1) 1\n    |> takeWhile ((>) 5)\n    --> [1, 2, 3, 4]" <|
        \() ->
            Expect.equal
                (
                iterate ((+) 1) 1
                |> takeWhile ((>) 5)
                )
                (
                [1, 2, 3, 4]
                )