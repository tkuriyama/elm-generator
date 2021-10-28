module Primes exposing (..)

{-| Examples of prime number generators.

  - Trial Division
  - Sieve of Eratosthenes

-}

import Dict
import Generator as G
import List.Extra as LE



--------------------------------------------------------------------------------


{-| Primes by trial division.

    G.take 10 trialDivisionWheel2
        == [ 2, 3, 5, 7, 9, 11, 13, 17, 19, 23, 29 ]

    G.take 10 trialDivisionWheel2357
        == [ 2, 3, 5, 7, 9, 11, 13, 17, 19, 23, 29 ]

The idea of using a hard-coded "wheel" for candidates to check comes from:
<https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>

-}
trialDivisionWheel2 =
    trialDivisionPrimes wheel2Init


trialDivisionWheel2357 =
    trialDivisionPrimes wheel2357Init



-- Implementation


type alias TrialDivisionState b =
    ( Int, List Int, G.Generator Int b )


trialDivisionPrimes :
    ( List Int, Int, G.Generator Int b )
    -> G.Generator Int ( TrialDivisionState b, List Int )
trialDivisionPrimes ( primes, lastPrime, candidateWheel ) =
    let
        state0 =
            ( lastPrime, List.reverse primes, candidateWheel )
    in
    G.init state0 trialDivisionNext
        |> G.prefix primes


trialDivisionNext : TrialDivisionState b -> Maybe ( Int, TrialDivisionState b )
trialDivisionNext ( lastPrime, primes, wheel ) =
    let
        ( guess, wheel_ ) =
            safeAdvance1 wheel

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


{-| Primes by an incremental Sieve of Eratosthenes.

The idea is to store the sieve's composite generators in a Dict, and update them just-in-time as more and more candidates are explored.

    G.take 10 Primes.incrementalSieveWheel2
        == [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 ]

    G.take 10 Primes.incrementalSieveWheel2357
        == [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 ]

See section 3 of <https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>

-}
incrementalSieveWheel2 =
    incrementalSievePrimes wheel2Init


incrementalSieveWheel2357 =
    incrementalSievePrimes wheel2357Init



-- Implementation


type alias IncrementalSieveState b =
    ( Int, GeneratorDict b, G.Generator Int b )


type alias GeneratorDict b =
    Dict.Dict Int (List (G.Generator Int b))


incrementalSievePrimes :
    ( List Int, Int, G.Generator Int b )
    -> G.Generator Int ( IncrementalSieveState b, List Int )
incrementalSievePrimes ( primes, lastPrime, candidateWheel ) =
    let
        state0 =
            ( lastPrime
            , insertNext lastPrime candidateWheel Dict.empty
            , candidateWheel
            )
    in
    G.init state0 incrementalSieveNext
        |> G.prefix primes


incrementalSieveNext :
    IncrementalSieveState b
    -> Maybe ( Int, IncrementalSieveState b )
incrementalSieveNext ( lastPrime, map, wheel ) =
    let
        ( guess, wheel_ ) =
            safeAdvance1 wheel
    in
    case Dict.get guess map of
        Nothing ->
            Just
                ( guess, ( guess, insertNext guess wheel_ map, wheel_ ) )

        Just composites ->
            incrementalSieveNext
                ( guess, updateMap guess composites map, wheel_ )


insertNext : Int -> G.Generator Int b -> GeneratorDict b -> GeneratorDict b
insertNext n generator =
    Dict.insert
        (n * n)
        [ G.map ((*) n) generator ]


updateMap :
    Int
    -> List (G.Generator Int b)
    -> GeneratorDict b
    -> GeneratorDict b
updateMap guess compositeGenerators =
    let
        reinsert compositeGenerator map_ =
            let
                ( nextComposite, compositeGenerator_ ) =
                    safeAdvance1 compositeGenerator
            in
            insertToList map_ nextComposite compositeGenerator_
    in
    Dict.remove guess
        >> (\map -> List.foldl reinsert map compositeGenerators)



--------------------------------------------------------------------------------
-- Seeds / candidates


wheel2 =
    G.repeat 2


wheel2Init =
    ( [ 2, 3 ], 3, wheel2 |> G.scanl (+) 3 )


wheel2357 =
    G.cycle [ 2, 4, 2, 4, 6, 2, 6, 4, 2, 4, 6, 6, 2, 6, 4, 2, 6, 4, 6, 8, 4, 2, 4, 2, 4, 8, 6, 4, 6, 2, 4, 6, 2, 6, 6, 4, 2, 4, 6, 2, 6, 4, 2, 4, 2, 10, 2, 10 ]


wheel2357Init =
    ( [ 2, 3, 5, 7, 11 ], 11, wheel2357 |> G.scanl (+) 11 )



--------------------------------------------------------------------------------
-- Helpers


insertToList :
    Dict.Dict comparable (List v)
    -> comparable
    -> v
    -> Dict.Dict comparable (List v)
insertToList map key val =
    case Dict.member key map of
        False ->
            Dict.insert key [ val ] map

        True ->
            Dict.update key (Maybe.map ((::) val)) map


safeAdvance1 : G.Generator Int b -> ( Int, G.Generator Int b )
safeAdvance1 =
    let
        unwrap =
            List.head >> Maybe.withDefault 0
    in
    G.advance 1 >> Tuple.mapFirst unwrap
