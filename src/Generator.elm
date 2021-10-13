module Generator exposing
    ( Generator
    , advance
    , filter
    , fromList
    , init
    , iterate
    , map
    , repeat
    , toList
    )

{-| -}


type Generator a b
    = Active (GeneratorRecord a b)
    | Empty


type alias GeneratorRecord a b =
    { state : b
    , next : b -> Maybe ( Maybe a, b )
    }



--------------------------------------------------------------------------------


init : b -> (b -> Maybe ( a, b )) -> Generator a b
init state genNext =
    let
        wrap f b =
            f b
                |> Maybe.map (\( a, b_ ) -> ( Just a, b_ ))
    in
    Active
        { state = state
        , next = wrap genNext
        }


repeat : a -> Generator a ()
repeat value =
    init () (\_ -> Just ( value, () ))


iterate : (a -> a) -> a -> Generator a a
iterate f value =
    init value (\value_ -> Just ( value_, f value_ ))



------------------


fromList : List a -> Generator a (List a)
fromList xs =
    case xs of
        [] ->
            Empty

        _ ->
            init xs splitList


splitList : List a -> Maybe ( a, List a )
splitList xs =
    case xs of
        y :: ys ->
            Just ( y, ys )

        [] ->
            Nothing


toList : Generator a b -> List a
toList generator =
    case next generator of
        ( Just value, generator_ ) ->
            value :: toList generator_

        ( Nothing, _ ) ->
            []



--------------------------------------------------------------------------------


advance : Int -> Generator a b -> ( List a, Generator a b )
advance n =
    advanceIter n []


advanceIter : Int -> List a -> Generator a b -> ( List a, Generator a b )
advanceIter n xs generator =
    case generator of
        Active g ->
            case n of
                0 ->
                    ( List.reverse xs, generator )

                _ ->
                    case next generator of
                        ( Just value, generator_ ) ->
                            advanceIter (n - 1) (value :: xs) generator_

                        ( Nothing, generator_ ) ->
                            advanceIter n xs generator_

        Empty ->
            ( List.reverse xs, Empty )


next : Generator a b -> ( Maybe a, Generator a b )
next generator =
    case generator of
        Active g ->
            case g.next g.state of
                Just ( value, state_ ) ->
                    ( value, Active { g | state = state_ } )

                Nothing ->
                    ( Nothing, Empty )

        Empty ->
            ( Nothing, Empty )



-- -----------------------------------------------------------------------------


map : (a -> c) -> Generator a b -> Generator c b
map f generator =
    case generator of
        Active g ->
            let
                next_ state =
                    g.next state
                        |> Maybe.map
                            (\( value, state_ ) ->
                                ( Maybe.map f value, state_ )
                            )
            in
            Active
                { state = g.state
                , next = next_
                }

        Empty ->
            Empty


filter : (a -> Bool) -> Generator a b -> Generator a b
filter f generator =
    case generator of
        Active g ->
            let
                predicate x =
                    case x of
                        Just x_ ->
                            if f x_ then
                                Just x_

                            else
                                Nothing

                        Nothing ->
                            Nothing

                next_ state =
                    g.next state
                        |> Maybe.map
                            (\( value, state_ ) ->
                                ( predicate value, state_ )
                            )
            in
            Active { g | next = next_ }

        Empty ->
            Empty
