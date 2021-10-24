module Generator exposing
    ( CycleGenerator
    , Generator
    , advance
    , cons
    , cycle
    , drop
    , dropWhile
    , empty
    , filter
    , foldl
    , fromList
    , head
    , init
    , interleave
    , intersperse
    , iterate
    , map
    , merge
    , mergeWith
    , prefix
    , repeat
    , scanl
    , tail
    , take
    , takeWhile
    , toList
    , zip
    , zipWith
    )

{-| This library provides a way to simulate lazy lists, or streams, in the form of generators.

Generators are constructed with some initial state and a `next` function, which takes the state and returns the next value to emit, as well as the successor state.

-}

import Internal.Types as Types exposing (..)
import Internal.Utils as Utils


type alias Generator a b =
    Types.Generator a b



--------------------------------------------------------------------------------
-- Constructors


{-| Construct a generator. Provide an initial state `b`, and a function that takes state and returns a `Maybe` (value, next state) tuple. Returning a `Nothing` signifiies that the generator is empty and will emit no further values.

     init 1 (\n -> Just (n, n+1))
     |> take 5
     --> [1, 2, 3, 4, 5]

-}
init : b -> (b -> Maybe ( a, b )) -> Generator a b
init state applyNext =
    let
        wrap f b =
            f b
                |> Maybe.map (\( a, b_ ) -> ( Just a, b_ ))
    in
    Active
        { state = state
        , next = wrap applyNext
        }


{-| An infinite generator that repeats the given value.

    repeat 1
    |> take 5
    --> [1, 1, 1, 1, 1]

-}
repeat : a -> Generator a ()
repeat value =
    init () (\_ -> Just ( value, () ))


{-| An infinite generator that repeatedly applies the given function to the starting value.

    iterate ((+) 1) 1
    |> take 5
    --> [1, 2, 3, 4, 5]

-}
iterate : (a -> a) -> a -> Generator a a
iterate f value =
    init value (\value_ -> Just ( value_, f value_ ))


{-| An infinite generator that repeated cycles through the given values.

    cycle [1, 2, 3]
    |> take 6
    --> [1, 2, 3, 1, 2, 3]

-}
type alias CycleGenerator a =
    Generator a ( List a, a, List a )


cycle : List a -> CycleGenerator a
cycle values =
    case values of
        [] ->
            Empty

        x :: xs ->
            init ( x :: xs, x, xs ) cycleHelper


cycleHelper : ( List a, a, List a ) -> Maybe ( a, ( List a, a, List a ) )
cycleHelper ( current, ref_head, ref_tail ) =
    case current of
        [] ->
            Just ( ref_head, ( ref_tail, ref_head, ref_tail ) )

        x :: xs ->
            Just ( x, ( xs, ref_head, ref_tail ) )


{-| Cons a value to a generator.

    cons "a" (fromList ["b", "c", "d"])
    |> take 4
    --> ["a", "b", "c", "d"]

-}
cons : a -> Generator a b -> Generator a ( b, List a )
cons value =
    prefix [ value ]


{-| Prefix a list of values to a generator.

    prefix [1, 2, 3] (fromList [4, 5, 6])
    |> take 6
    --> [1, 2, 3, 4, 5, 6]

-}
prefix : List a -> Generator a b -> Generator a ( b, List a )
prefix values0 =
    let
        next_ applyNext ( state, values ) =
            case values of
                x :: xs ->
                    Just ( Just x, ( state, xs ) )

                [] ->
                    applyNext state
                        |> Maybe.map
                            (\( value, state_ ) ->
                                ( value, ( state_, [] ) )
                            )
    in
    Utils.bind
        (\g ->
            Active
                { state =
                    ( g.state
                    , values0
                    )
                , next = next_ g.next
                }
        )



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
next =
    let
        default =
            ( Nothing, Empty )

        applyNext g =
            case g.next g.state of
                Just ( value, state_ ) ->
                    ( value, Active { g | state = state_ } )

                Nothing ->
                    default
    in
    Utils.withDefault default applyNext


{-| Advance one step and return the emitted value (or `Nothing` if the generator is empty).

    repeat 1
    |> head
    --> Just 1

-}
head : Generator a b -> Maybe a
head =
    let
        tryHead g =
            advance 1 (Active g)
                |> Tuple.first
                |> List.head
    in
    Utils.withDefault Nothing tryHead


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


{-| Take items emitted by the generator while the given predicate holds.

    iterate ((+) 1) 1
    |> takeWhile ((>) 5)
    --> [1, 2, 3, 4]

-}
takeWhile : (a -> Bool) -> Generator a b -> List a
takeWhile predicate generator =
    case ( head generator, tail generator ) of
        ( Just x, generator_ ) ->
            case predicate x of
                True ->
                    x :: takeWhile predicate generator_

                False ->
                    []

        ( _, _ ) ->
            []


{-| Advance the generator by n steps, dropping the emitted values. Convenience function for `advance n >> Tuple.second`.

    iterate ((+) 1) 1
    |> drop 5
    |> take 5
    --> [6, 7, 8, 9, 10]

-}
drop : Int -> Generator a b -> Generator a b
drop n =
    advance n >> Tuple.second


{-| Drop items emitted by the generator while the given predicate holds.

    iterate ((+) 1) 1
    |> dropWhile ((>) 5)
    |> take 5
    --> [6, 7, 8, 9, 10]

-}
dropWhile : (a -> Bool) -> Generator a b -> Generator a b
dropWhile predicate generator =
    case ( head generator, tail generator ) of
        ( Just x, generator_ ) ->
            case predicate x of
                True ->
                    dropWhile predicate generator_

                False ->
                    generator_

        ( _, generator_ ) ->
            generator_



{- Test if a generator is empty. An empty generator will emit no further values. Note that it's not necessary to check for emptiness before calling `advance` or other functions that attempt to advance the generator.

   fromList [1, 2, 3, 4, 5]
   |> drop 5
   |> empty
   --> True
-}


empty : Generator a b -> Bool
empty =
    Utils.withDefault True (\_ -> False)



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
        next_ applyNext state =
            applyNext state
                |> Maybe.map
                    (\( value, state_ ) ->
                        ( Maybe.map f value, state_ )
                    )
    in
    Utils.bind (\g -> Active { state = g.state, next = next_ g.next })


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
        next_ applyNext state =
            applyNext state
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
    Utils.bind (\g -> Active { g | next = next_ g.next })


{-| Foldl for genereators: return a new generator that successively reduces emitted values.

    fromList [ 1, 2, 3 ]
    |> scanl (+) 0
    |> take 4
    --> [1, 3, 6]

-}
scanl : (c -> a -> c) -> c -> Generator a b -> Generator c ( b, c )
scanl f acc0 =
    let
        next_ applyNext ( state, acc ) =
            case Utils.getNextValue applyNext state of
                Just ( Just value, state_ ) ->
                    Just ( Just <| f acc value, ( state_, f acc value ) )

                _ ->
                    Nothing
    in
    Utils.bind (\g -> Active { state = ( g.state, acc0 ), next = next_ g.next })



--------------------------------------------------------------------------------
-- Combining Generators


{-| Return a new generator that combines values emitted by two generators into pairs.

    iterate ((+) 1) 1
    |> (\g -> zip g g)
    |> take 3
    --> [(1, 1), (2, 2), (3, 3)]

-}
zip :
    Generator a b
    -> Generator c d
    -> Generator ( a, c ) ( b, d )
zip generator1 generator2 =
    zipWith Tuple.pair generator1 generator2


{-| Return a new generator that combines values emitted by two generators with the given function.

    iterate ((+) 1) 1
    |> (\g -> zipWith (+) g g)
    |> take 5
    --> [2, 4, 6, 8, 10]

    iterate ((+) 1) 1
    |> (\g -> zipWith (+) g (filter ((<) 5) g))
    |> take 5
    --> [7, 9, 11, 13, 15]

-}
zipWith :
    (a -> c -> e)
    -> Generator a b
    -> Generator c d
    -> Generator e ( b, d )
zipWith f generator1 generator2 =
    Utils.bind2 (zipWithHelper f) generator1 generator2


zipWithHelper :
    (a -> c -> e)
    -> GeneratorRecord a b
    -> GeneratorRecord c d
    -> Generator e ( b, d )
zipWithHelper f g1 g2 =
    let
        nextVal =
            Utils.getNextValue

        next_ g1Next g2Next ( state1, state2 ) =
            case ( nextVal g1Next state1, nextVal g2Next state2 ) of
                ( Just ( Just value1, state1_ ), Just ( Just value2, state2_ ) ) ->
                    Just ( Just <| f value1 value2, ( state1_, state2_ ) )

                ( _, _ ) ->
                    Nothing
    in
    Active
        { state = ( g1.state, g2.state )
        , next = next_ g1.next g2.next
        }


{-| Return a new generator that merges two generators.

The merge rule is a predicate function that compares the values emitted by the two generators and chooses the left value if the predicate is true. Only the generator with the chosen value will be advanced.

    fromList [ 1, 3, 4, 8 ]
    |> (\g1 -> ( g1, fromList [ 2, 3, 5, 7 ] ))
    |> (\(g1, g2) -> merge (<) g1 g2)
    |> toList
    --> [1, 2, 3, 3, 4, 5, 7, 8]

-}
merge :
    (a -> a -> Bool)
    -> Generator a b
    -> Generator a d
    -> Generator a ( b, d )
merge chooseLeft generator1 generator2 =
    Utils.bind2 (mergeHelper chooseLeft) generator1 generator2


mergeHelper :
    (a -> a -> Bool)
    -> GeneratorRecord a b
    -> GeneratorRecord a d
    -> Generator a ( b, d )
mergeHelper chooseLeft g1 g2 =
    let
        nextVal =
            Utils.getNextValue

        next_ g1Next g2Next ( state1, state2 ) =
            case ( nextVal g1Next state1, nextVal g2Next state2 ) of
                ( Just ( Just value1, state1_ ), Just ( Just value2, state2_ ) ) ->
                    case chooseLeft value1 value2 of
                        True ->
                            Just ( Just value1, ( state1_, state2 ) )

                        False ->
                            Just ( Just value2, ( state1, state2_ ) )

                ( Just ( Just value1, state1_ ), _ ) ->
                    Just ( Just value1, ( state1_, state2 ) )

                ( _, Just ( Just value2, state2_ ) ) ->
                    Just ( Just value2, ( state1, state2_ ) )

                ( _, _ ) ->
                    Nothing
    in
    Active
        { state = ( g1.state, g2.state )
        , next = next_ g1.next g2.next
        }


{-| Return a new generator that merges two generators with a custom function.

This is a more expressive way to merge two generators, compared to the simple left or right choice that `merge` provides. The merge function takes two values and returns a triple of (merged value, bool to advance left generator, bool to advance the right generator). Since the new generator may emit values of any type, additional functions are required to convert the left and right generator values to the merged value type (in case either generator is empty).

-}
mergeWith :
    (a -> c -> ( e, Bool, Bool ))
    -> (a -> e)
    -> (c -> e)
    -> Generator a b
    -> Generator c d
    -> Generator e ( b, d )
mergeWith applyMerge applyLeft applyRight generator1 generator2 =
    Utils.bind2
        (mergeWithHelper applyMerge applyLeft applyRight)
        generator1
        generator2


mergeWithHelper :
    (a -> c -> ( e, Bool, Bool ))
    -> (a -> e)
    -> (c -> e)
    -> GeneratorRecord a b
    -> GeneratorRecord c d
    -> Generator e ( b, d )
mergeWithHelper applyMerge applyLeft applyRight g1 g2 =
    let
        nextVal =
            Utils.getNextValue

        next_ g1Next g2Next ( state1, state2 ) =
            case ( nextVal g1Next state1, nextVal g2Next state2 ) of
                ( Just ( Just value1, state1_ ), Just ( Just value2, state2_ ) ) ->
                    case applyMerge value1 value2 of
                        ( value, True, True ) ->
                            Just ( Just value, ( state1_, state2_ ) )

                        ( value, True, False ) ->
                            Just ( Just value, ( state1_, state2 ) )

                        ( value, False, True ) ->
                            Just ( Just value, ( state1, state2_ ) )

                        ( value, False, False ) ->
                            Just ( Just value, ( state1, state2 ) )

                ( Just ( Just value1, state1_ ), _ ) ->
                    Just ( Just <| applyLeft value1, ( state1_, state2 ) )

                ( _, Just ( Just value2, state2_ ) ) ->
                    Just ( Just <| applyRight value2, ( state1, state2_ ) )

                ( _, _ ) ->
                    Nothing
    in
    Active
        { state = ( g1.state, g2.state )
        , next = next_ g1.next g2.next
        }


{-| Return a new generator that intersperses values from the given generator with the given constant value.

    intersperse "." (fromList ["a", "b", "c"])
    |> take 6
    --> ["a", ".", "b", ".", "c", "."]

-}
intersperse : a -> Generator a b -> Generator a ( b, (), Bool )
intersperse value generator =
    interleave generator (repeat value)


{-| Return a new generator that alternates between values emitted by the two given generators.

    interleave (repeat 1) (repeat 2)
    |> take 6
    --> [1, 2, 1, 2, 1, 2]

-}
interleave :
    Generator a b
    -> Generator a c
    -> Generator a ( b, c, Bool )
interleave generator1 generator2 =
    Utils.bind2 interleaveHelper generator1 generator2


interleaveHelper :
    GeneratorRecord a b
    -> GeneratorRecord a c
    -> Generator a ( b, c, Bool )
interleaveHelper g1 g2 =
    let
        next_ g1Next g2Next ( state1, state2, left ) =
            case left of
                True ->
                    case Utils.getNextValue g1Next state1 of
                        Just ( Just value1, state1_ ) ->
                            Just ( Just value1, ( state1_, state2, False ) )

                        _ ->
                            Nothing

                False ->
                    case Utils.getNextValue g2Next state2 of
                        Just ( Just value2, state2_ ) ->
                            Just ( Just value2, ( state1, state2_, True ) )

                        _ ->
                            Nothing
    in
    Active
        { state = ( g1.state, g2.state, True )
        , next = next_ g1.next g2.next
        }



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
