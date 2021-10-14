module VerifyExamples.Generator.Map0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generator exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#map: \n\n    iterate ((+) 1) 1\n    |> map ((+) 1)\n    |> map ((+) 1)\n    |> take 5\n    --> [3, 4, 5, 6, 7]" <|
        \() ->
            Expect.equal
                (
                iterate ((+) 1) 1
                |> map ((+) 1)
                |> map ((+) 1)
                |> take 5
                )
                (
                [3, 4, 5, 6, 7]
                )