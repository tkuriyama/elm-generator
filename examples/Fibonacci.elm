module Fibonacci exposing (..)

{-| Example fibonacci sequence implementations.
-}

import Generator as G



--------------------------------------------------------------------------------


{-| Example 1: using the `iterate` interface`and`map\` to reshape the output.
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
-}
fib2 : G.Generator Int ( Int, Int )
fib2 =
    G.init
        ( 0, 1 )
        (\( a, b ) -> Just ( a, ( b, b + a ) ))
