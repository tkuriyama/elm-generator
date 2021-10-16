module Internal.Utils exposing (..)

import Internal.Types exposing (..)



--------------------------------------------------------------------------------


bind :
    (GeneratorRecord a b -> Generator c d)
    -> Generator a b
    -> Generator c d
bind f generator =
    case generator of
        Active g ->
            f g

        Empty ->
            Empty


bind2 :
    (GeneratorRecord a b -> GeneratorRecord c d -> Generator e f)
    -> Generator a b
    -> Generator c d
    -> Generator e f
bind2 f generator1 generator2 =
    case ( generator1, generator2 ) of
        ( Active g1, Active g2 ) ->
            f g1 g2

        ( _, _ ) ->
            Empty


withDefault :
    c
    -> (GeneratorRecord a b -> c)
    -> Generator a b
    -> c
withDefault default f generator =
    case generator of
        Active g ->
            f g

        Empty ->
            default



--------------------------------------------------------------------------------


getNextValue :
    (b -> Maybe ( Maybe a, b ))
    -> b
    -> Maybe ( Maybe a, b )
getNextValue applyNext state =
    case applyNext state of
        Just ( Just value, state_ ) ->
            Just ( Just value, state_ )

        Just ( Nothing, state_ ) ->
            getNextValue applyNext state_

        Nothing ->
            Nothing
