module Internal.Types exposing (..)

--------------------------------------------------------------------------------


type Generator a b
    = Active (GeneratorRecord a b)
    | Empty


type alias GeneratorRecord a b =
    { state : b
    , next : b -> Maybe ( Maybe a, b )
    }
