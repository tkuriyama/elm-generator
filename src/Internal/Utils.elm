module Internal.Utils exposing (..)

import Internal.Types exposing (..)



--------------------------------------------------------------------------------


bind :
    (GeneratorRecord a b -> Generator c b)
    -> Generator a b
    -> Generator c b
bind f generator =
    case generator of
        Active g ->
            f g

        Empty ->
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
