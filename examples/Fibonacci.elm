module Fibonacci exposing (..)

import Generator as G



--------------------------------------------------------------------------------


fibIter : G.Generator ( Int, Int ) ( Int, Int )
fibIter =
    G.iterate
        (\( a, b ) -> ( b, b + a ))
        ( 0, 1 )


fib1 : G.Generator Int ( Int, Int )
fib1 =
    G.map (\( a, b ) -> a) fibIter



--------------------------------------------------------------------------------


fib2 : G.Generator Int ( Int, Int )
fib2 =
    G.init
        ( 0, 1 )
        (\( a, b ) -> Just ( a, ( b, b + a ) ))
