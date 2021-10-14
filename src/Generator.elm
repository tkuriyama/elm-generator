module Generator exposing
    ( Generator
    , advance
    , drop
    , filter
    , foldl
    , fromList
    , head
    , init
    , iterate
    , map
    , repeat
    , tail
    , take
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
-- Constructors


{-| Construct a generator. Provide an initial state `b`, and a function that takes state and returns a `Maybe` (value, next state) tuple. Returning a `Nothing` signifiies that the generator is empty and will emit no further values.

     init 1 (\n -> Just (n, n+1))
     |> take 5
     --> [1, 2, 3, 4, 5]

-}
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


{-| An infinite generator that repeats the given value.

    repeat 1
    |> take 5
    --> [1, 1, 1, 1, 1]

-}
repeat : a -> Generator a ()
repeat value =
    init () (\_ -> Just ( value, () ))


{-| An infinite generator that repeatedly applies the given function.

    iterate ((+) 1) 1
    |> take 5
    --> [1, 2, 3, 4, 5]

-}
iterate : (a -> a) -> a -> Generator a a
iterate f value =
    init value (\value_ -> Just ( value_, f value_ ))



--------------------------------------------------------------------------------
-- Basic Manipulation


{-| Advance a generator by a given nubmer of steps and collect emitted values. Returns the list of collected values and the updated generated as a pair.

    iterate ((+) 1) 1
    |> advance 5       -- ([1, 2, 3, 4, 5], updated generator)
    |> Tuple.second
    |> advance 5        -- ([6, 7, 8, 9, 10], updated generator)
    |> Tuple.first
    --> [6, 7, 8, 9, 10]

-}
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


{-| Advance one step and return the emitted value (or `Nothing` if the generator is empty).

    repeat 1
    |> head
    --> Just 1

-}
head : Generator a b -> Maybe a
head generator =
    case generator of
        Active g ->
            advance 1 (Active g)
                |> Tuple.first
                |> List.head

        Empty ->
            Nothing


{-| Advance one step and return the updated generator.

    iterate ((+) 1) 1
    |> tail
    |> head
    --> Just 2

-}
tail : Generator a b -> Generator a b
tail =
    advance 1 >> Tuple.second


{-| Take the first n items emitted by the generator. Convenience function for `advance n >> Tuple.first`.

    iterate ((+) 1) 1
    |> take 5
    --> [1, 2, 3, 4, 5]

-}
take : Int -> Generator a b -> List a
take n =
    advance n >> Tuple.first


{-| Advance the generator by n steps, dropping the emitted values. Convenience function for `advance n >> Tuple.second`.

    iterate ((+) 1) 1
    |> drop 5
    |> take 5
    --> [6, 7, 8, 9, 10]

-}
drop : Int -> Generator a b -> Generator a b
drop n =
    advance n >> Tuple.second



{- Test if a generator is empty. An empty generator will emit no further values. Note that it's not necessary to check for emptiness before calling `advance` or other functions that attempt to advance the generator.

   fromList [1, 2, 3, 4, 5]
   |> drop 5
   |> empty
   --> True
-}


empty : Generator a b -> Bool
empty generator =
    case generator of
        Active _ ->
            False

        Empty ->
            True



-- -----------------------------------------------------------------------------
-- Transformations


{-| Return a new generator that maps the given function to every value emitted.

    iterate ((+) 1) 1
    |> map ((+) 1)
    |> map ((+) 1)
    |> take 5
    --> [3, 4, 5, 6, 7]

-}
map : (a -> c) -> Generator a b -> Generator c b
map f =
    let
        next_ g state =
            g.next state
                |> Maybe.map
                    (\( value, state_ ) ->
                        ( Maybe.map f value, state_ )
                    )
    in
    bind (\g -> Active { state = g.state, next = next_ g })


{-| Return a new generator that filters every value emitted.

    iterate ((+) 1) 1
    |> map ((+) 1)
    |> filter ((<) 3)
    |> take 5
    --> [4, 5, 6, 7, 8]

-}
filter : (a -> Bool) -> Generator a b -> Generator a b
filter f =
    let
        next_ g state =
            g.next state
                |> Maybe.map
                    (\( value, state_ ) ->
                        ( predicate value, state_ )
                    )

        predicate x =
            case x of
                Just x_ ->
                    if f x_ then
                        Just x_

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    bind (\g -> Active { g | next = next_ g })



------------------
-- Finite Generators


{-| Construct a finite generator.

    fromList [ 1, 2, 3, 4, 5 ]
    |> advance 5
    |> Tuple.first
    --> [1, 2, 3, 4, 5]

-}
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


{-| Attempt to collect all values emitted from a generator into a list.

    fromList [ 1, 2, 3, 4, 5 ]
    |> toList
    --> [1, 2, 3, 4, 5]

**Note**: this function runs forever if the generater is infinite.

-}
toList : Generator a b -> List a
toList generator =
    foldl (::) [] generator
        |> List.reverse


{-| Attempt to reduce all values emitted from a generator from the left.

    fromList [ 1, 2, 3, 4, 5 ]
    |> foldl (::) []
    --> [ 5, 4, 3, 2, 1 ]

    fromList [ 1, 2, 3, 4, 5 ]
    |> foldl (+) 0
    --> 15

**Note**: this function runs forever if the generater is infinite.

-}
foldl : (a -> c -> c) -> c -> Generator a b -> c
foldl f acc generator =
    case generator of
        Active _ ->
            case advance 1 generator of
                ( a :: [], generator_ ) ->
                    foldl f (f a acc) generator_

                _ ->
                    acc

        Empty ->
            acc



--------------------------------------------------------------------------------
-- Internal Helpers


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
