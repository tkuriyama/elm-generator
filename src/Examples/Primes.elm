module Examples.Primes exposing (..)

{-| Examples of prime number generators.

  - Trial Division
  - Sieve of Eratosthenes

See <https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf> for further details on the algorithms.

-}

import Dict
import Generator as G
import List.Extra as LE



--------------------------------------------------------------------------------


{-| Primes by trial division.

    import Generator as G

    trialDivisionWheel2
    |> G.take 10
    --> [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 ]

    trialDivisionWheel2357
    |> G.take 10
    --> [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 ]

The idea of using a hard-coded "wheel" for candidates to check comes from:
<https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>

-}
trialDivisionWheel2 =
    trialDivisionPrimes wheel2Init


trialDivisionWheel2357 =
    trialDivisionPrimes wheel2357Init



-- Implementation


type alias TrialDivisionState b =
    ( List Int, G.Generator Int b )


trialDivisionPrimes :
    ( List Int, Int, G.Generator Int b )
    -> G.Generator Int ( TrialDivisionState b, List Int )
trialDivisionPrimes ( primes, lastPrime, candidateWheel ) =
    let
        state0 =
            ( List.reverse primes, candidateWheel )
    in
    G.init state0 trialDivisionNext
        |> G.prefix primes


trialDivisionNext : TrialDivisionState b -> Maybe ( Int, TrialDivisionState b )
trialDivisionNext ( primes, wheel ) =
    let
        ( guess, wheel_ ) =
            safeAdvance1 wheel

        primes_ =
            LE.dropWhile (\p -> p * p > guess) primes
    in
    case trialDivisionPrime guess primes_ of
        True ->
            Just ( guess, ( guess :: primes, wheel_ ) )

        False ->
            trialDivisionNext ( primes, wheel_ )


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

    import Generator as G

    incrementalSieveWheel2
    |> G.take 10
    --> [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 ]

    incrementalSieveWheel2357
    |> G.take 10
     --> [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29 ]

See section 3 of <https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>

-}
incrementalSieveWheel2 =
    incrementalSievePrimes wheel2Init


incrementalSieveWheel2357 =
    incrementalSievePrimes wheel2357Init



-- Implementation


type alias IncrementalSieveState b =
    ( GeneratorDict b, G.Generator Int b )


type alias GeneratorDict b =
    Dict.Dict Int (List (G.Generator Int b))


incrementalSievePrimes :
    ( List Int, Int, G.Generator Int b )
    -> G.Generator Int ( IncrementalSieveState b, List Int )
incrementalSievePrimes ( primes, lastPrime, candidateWheel ) =
    let
        state0 =
            ( insertNext lastPrime candidateWheel Dict.empty
            , candidateWheel
            )
    in
    G.init state0 incrementalSieveNext
        |> G.prefix primes


incrementalSieveNext :
    IncrementalSieveState b
    -> Maybe ( Int, IncrementalSieveState b )
incrementalSieveNext ( map, wheel ) =
    let
        ( guess, wheel_ ) =
            safeAdvance1 wheel
    in
    case Dict.get guess map of
        Nothing ->
            Just
                ( guess, ( insertNext guess wheel_ map, wheel_ ) )

        Just composites ->
            incrementalSieveNext
                ( updateComposites guess composites map, wheel_ )


insertNext : Int -> G.Generator Int b -> GeneratorDict b -> GeneratorDict b
insertNext n generator =
    Dict.insert
        (n * n)
        [ G.map ((*) n) generator ]


updateComposites :
    Int
    -> List (G.Generator Int b)
    -> GeneratorDict b
    -> GeneratorDict b
updateComposites guess compositeGenerators =
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
-- Seeds / Candidates


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
