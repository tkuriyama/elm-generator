module VerifyExamples.Examples.Primes.TrialDivisionWheel21 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Examples.Primes exposing (..)
import Generator as G







spec1 : Test.Test
spec1 =
    Test.test "#trialDivisionWheel2: \n\n    trialDivisionWheel2\n    |> G.take 10\n    --> [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 ]" <|
        \() ->
            Expect.equal
                (
                trialDivisionWheel2
                |> G.take 10
                )
                (
                [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 ]
                )