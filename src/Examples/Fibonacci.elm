module Examples.Fibonacci exposing (..)

{-| Example fibonacci sequence implementations.
-}

import Generator as G



--------------------------------------------------------------------------------


{-| Example 1: using the `iterate` interface`and`map\` to reshape the output.

    import Generator as G

    fib1
    |> G.take 10
    --> [0,1,1,2,3,5,8,13,21,34]

-}
fibIter : G.Generator ( Int, Int ) ( Int, Int )
fibIter =
    G.iterate
        (\( a, b ) -> ( b, b + a ))
        ( 0, 1 )


fib1 : G.Generator Int ( Int, Int )
fib1 =
    G.map (\( a, b ) -> a) fibIter


{-| Example 2: using the `init` interface directly.

    import Generator as G

    fib2
    |> G.take 10
    --> [0,1,1,2,3,5,8,13,21,34]

-}
fib2 : G.Generator Int ( Int, Int )
fib2 =
    G.init
        ( 0, 1 )
        (\( a, b ) -> Just ( a, ( b, b + a ) ))
