module Examples.Collatz exposing (..)

{- } Implementation of the Collatz sequence.

   If the Collatz conjecture holds, `collatz n |> G.toList` should always terminate!

-}

import Generator as G



--------------------------------------------------------------------------------


{-| Collatz sequence generator.

    import Generator as G

    collatz 10 |> G.toList
    --> [10,5,16,8,4,2,1]

-}
collatz : Int -> G.Generator Int Int
collatz n =
    G.init
        n
        collatzNext


collatzNext : Int -> Maybe ( Int, Int )
collatzNext n =
    case n <= 0 of
        True ->
            Nothing

        False ->
            case ( n == 1, modBy 2 n == 0 ) of
                ( True, _ ) ->
                    Just ( 1, 0 )

                ( False, True ) ->
                    Just ( n, n // 2 )

                ( False, False ) ->
                    Just ( n, n * 3 + 1 )
