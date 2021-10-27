module Primes exposing (..)

{-| Exampls of prime number generators.
-}

import Generator as G
import List.Extra as LE



--------------------------------------------------------------------------------


{-| Primes by trial division.

    > trialDivisionNaive |> G.take 10
    [2,3,5,7,9,11,13,17,19,23,29]

    > trialDivisionWheel |> G.take 10
    [2,3,5,7,9,11,13,17,19,23,29]

The idea of using a "wheel" for candidates to check comes from:
<https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>

-}
trialDivisionNaive =
    trialDivisionPrimes [ 2, 3 ] wheel2


trialDivisionWheel =
    trialDivisionPrimes [ 2, 3, 5, 7, 11 ] wheel2357


trialDivisionPrimes :
    List Int
    -> G.Generator Int b
    -> G.Generator Int ( ( Int, List Int, G.Generator Int b ), List Int )
trialDivisionPrimes seed incrementWheel =
    let
        state0 =
            ( LE.last seed |> Maybe.withDefault 0
            , List.reverse seed
            , incrementWheel
            )
    in
    G.init state0 trialDivisionNext
        |> G.prefix seed


trialDivisionNext :
    ( Int, List Int, G.Generator Int b )
    -> Maybe ( Int, ( Int, List Int, G.Generator Int b ) )
trialDivisionNext ( lastPrime, primes, wheel ) =
    let
        ( inc, wheel_ ) =
            G.advance 1 wheel

        guess =
            lastPrime + (List.head inc |> Maybe.withDefault 0)

        primes_ =
            LE.dropWhile (\p -> p * p > guess) primes
    in
    case trialDivisionPrime guess primes_ of
        True ->
            Just ( guess, ( guess, guess :: primes, wheel_ ) )

        False ->
            trialDivisionNext ( guess, primes, wheel_ )


trialDivisionPrime : Int -> List Int -> Bool
trialDivisionPrime guess primes =
    case primes of
        [] ->
            True

        x :: xs ->
            case modBy x guess > 0 of
                True ->
                    trialDivisionPrime guess xs

                False ->
                    False


wheel2 =
    G.repeat 2


wheel2357 =
    G.cycle [ 2, 4, 2, 4, 6, 2, 6, 4, 2, 4, 6, 6, 2, 6, 4, 2, 6, 4, 6, 8, 4, 2, 4, 2, 4, 8, 6, 4, 6, 2, 4, 6, 2, 6, 6, 4, 2, 4, 6, 2, 6, 4, 2, 4, 2, 10, 2, 10 ]
