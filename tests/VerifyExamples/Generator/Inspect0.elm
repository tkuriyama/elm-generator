module VerifyExamples.Generator.Inspect0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generator exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#inspect: \n\n    fromList [1, 2, 3, 4, 5]\n    |> drop 2\n    |> inspect\n    --> Just [3, 4, 5]" <|
        \() ->
            Expect.equal
                (
                fromList [1, 2, 3, 4, 5]
                |> drop 2
                |> inspect
                )
                (
                Just [3, 4, 5]
                )