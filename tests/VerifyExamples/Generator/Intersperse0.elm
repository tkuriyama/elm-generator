module VerifyExamples.Generator.Intersperse0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Generator exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#intersperse: \n\n    intersperse \".\" (fromList [\"a\", \"b\", \"c\"])\n    |> take 6\n    --> [\"a\", \".\", \"b\", \".\", \"c\", \".\"]" <|
        \() ->
            Expect.equal
                (
                intersperse "." (fromList ["a", "b", "c"])
                |> take 6
                )
                (
                ["a", ".", "b", ".", "c", "."]
                )