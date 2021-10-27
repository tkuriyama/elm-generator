module Primes exposing (..)

import Generator as G
import List.Extra as LE



--------------------------------------------------------------------------------
{- Trial division, aka the "fake" Sieve of Eratosthenes.

   Sometimes there are implementations of the Sieve of Eratosthenes along these lines, which is in fact a trial division algorithm.
-}


trialDivisionPrimes =
    let
        next ( lastPrime, primes ) =
            nextTrialDivisionPrime lastPrime primes
                |> (\p -> Just ( p, ( p, p :: primes ) ))
    in
    G.prefix [ 2, 3 ] (G.init ( 3, [ 3 ] ) next)


nextTrialDivisionPrime : Int -> List Int -> Int
nextTrialDivisionPrime lastPrime primes =
    let
        guess =
            lastPrime + 2

        primes_ =
            LE.dropWhile (\p -> p * p >= guess) primes
    in
    case trialDivision guess primes_ of
        True ->
            guess

        False ->
            nextTrialDivisionPrime guess primes


trialDivision : Int -> List Int -> Bool
trialDivision p primes =
    case primes of
        [] ->
            True

        x :: xs ->
            case modBy x p > 0 of
                True ->
                    trialDivision p xs

                False ->
                    False
