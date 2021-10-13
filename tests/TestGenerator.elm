module TestGenerator exposing (..)

import Expect exposing (Expectation)
import Generator as G
import Test exposing (..)



--------------------------------------------------------------------------------


testSetup : Test
testSetup =
    let
        g =
            { value = 1
            , ctr = 0
            , next = \x -> x + 1
            }
    in
    describe "Test generator setup"
        [ test "setup" (\g -> Expect.equal 1)
        ]
