module VerifyExamples.Generator.Foldl0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generator exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#foldl: \n\n    fromList [ 1, 2, 3, 4, 5 ]\n    |> foldl (+) 0\n    --> 15" <|
        \() ->
            Expect.equal
                (
                fromList [ 1, 2, 3, 4, 5 ]
                |> foldl (+) 0
                )
                (
                15
                )