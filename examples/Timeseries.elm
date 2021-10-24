module Timeseries exposing (..)

{-| An example representing simple timeseries with generators.

The `Generator.merge` and `Generator.mergeWith` functions are wrapped to implement merging and aligning functionality specific to timeseries, as in the `merged` example.

    > T.merged |> T.showTimeseriesSet 10
            One     Two     Three
    1       1.00    1.00    -
     2       1.10    1.10    1.10
     3       -       1.30    1.30
     4       1.50    -       1.35
     5       -       1.40    1.40
     6       -       -       1.70

-}

import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Generator as G
import String.Format as Fmt



--------------------------------------------------------------------------------
{- For simplicity of example, time is represented by an Int. -}


type alias Timeseries b =
    G.Generator ( Int, Maybe Float ) b


type alias TimeseriesSet b =
    { names : List String
    , set : G.Generator ( Int, List (Maybe Float) ) b
    }



--------------------------------------------------------------------------------
{- Example data -}


merged =
    merge timeseries3 timeseries4
        |> toSet "Three"
        |> alignToSet "Two" timeseries2
        |> alignToSet "One" timeseries1


timeseries1 : Timeseries (List ( Int, Maybe Float ))
timeseries1 =
    G.fromList
        [ ( 1, Just 1.0 )
        , ( 2, Just 1.1 )
        , ( 4, Just 1.5 )
        ]


timeseries2 : Timeseries (List ( Int, Maybe Float ))
timeseries2 =
    G.fromList
        [ ( 1, Just 1.0 )
        , ( 2, Just 1.1 )
        , ( 3, Just 1.3 )
        , ( 5, Just 1.4 )
        ]


timeseries3 : Timeseries (List ( Int, Maybe Float ))
timeseries3 =
    G.fromList
        [ ( 2, Just 1.1 )
        , ( 3, Just 1.3 )
        , ( 5, Just 1.4 )
        ]


timeseries4 : Timeseries (List ( Int, Maybe Float ))
timeseries4 =
    G.fromList
        [ ( 4, Just 1.35 )
        , ( 6, Just 1.7 )
        ]



--------------------------------------------------------------------------------
{- Merge two ordered timeseries. -}


merge : Timeseries b -> Timeseries c -> Timeseries ( b, c )
merge tsLeft tsRight =
    G.merge
        (\l r -> Tuple.first l < Tuple.first r)
        tsLeft
        tsRight



{- Align two ordered timeseries. -}


toSet : String -> Timeseries b -> TimeseriesSet b
toSet name ts =
    { names = [ name ]
    , set = G.map (\( timestamp, value ) -> ( timestamp, [ value ] )) ts
    }


alignToSet :
    String
    -> Timeseries b
    -> TimeseriesSet c
    -> TimeseriesSet ( b, c )
alignToSet name ts tsSet =
    { names = name :: tsSet.names
    , set = alignToSetHelper (List.length tsSet.names) ts tsSet.set
    }


alignToSetHelper :
    Int
    -> Timeseries b
    -> G.Generator ( Int, List (Maybe Float) ) c
    -> G.Generator ( Int, List (Maybe Float) ) ( b, c )
alignToSetHelper length g gSet =
    let
        applyMerge ( time, value ) ( time2, values ) =
            case ( time == time2, time > time2 ) of
                ( True, _ ) ->
                    ( ( time, value :: values ), True, True )

                ( False, True ) ->
                    ( ( time2, Nothing :: values ), False, True )

                ( False, False ) ->
                    ( ( time, value :: List.repeat length Nothing ), True, False )

        leftIdentity ( time, value ) =
            ( time, value :: List.repeat length Nothing )

        rightIdentity ( time, values ) =
            ( time, Nothing :: values )
    in
    G.mergeWith applyMerge leftIdentity rightIdentity g gSet



{- TimeseriesSet -> String -}


showTimeseriesSet : Int -> TimeseriesSet b -> String
showTimeseriesSet n tsSet =
    "{{header}}\n{{body}}"
        |> Fmt.namedValue "header" (alignLeftList 8 ("" :: tsSet.names))
        |> Fmt.namedValue "body" (G.take n tsSet.set |> showData 8)


showData : Int -> List ( Int, List (Maybe Float) ) -> String
showData n =
    List.map (showRow n)
        >> List.intersperse "\n"
        >> String.concat


showRow : Int -> ( Int, List (Maybe Float) ) -> String
showRow n ( time, values ) =
    let
        values_ =
            List.map showValue values
                |> String.concat

        showValue val =
            case val of
                Just v ->
                    alignLeft n (fmtFloat v)

                Nothing ->
                    alignLeft n "-"
    in
    "{{timestamp}}{{alignedValues}}"
        |> Fmt.namedValue "timestamp" (alignLeft 8 <| String.fromInt time)
        |> Fmt.namedValue "alignedValues" values_


alignLeftList : Int -> List String -> String
alignLeftList n =
    List.map (alignLeft n) >> String.concat


alignLeft : Int -> String -> String
alignLeft n s =
    case String.length s >= n of
        True ->
            s

        False ->
            s ++ String.repeat (n - String.length s) " "


fmtFloat : Float -> String
fmtFloat =
    FormatNumber.format usLocale
