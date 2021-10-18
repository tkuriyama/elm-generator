module Finance exposing (..)

{-| Examples modeling basic financial math.
-}

import Generator as G



--------------------------------------------------------------------------------
{- Interest rate compounding.

     After n periods, a unit investment of 1 will have observed the compound growth of (1 * r_1) * r_2 * ... r_n

   For example, an interest rate of 6% will double the initial investment in approx 12 years:

    G.take 12 (compound 1.06) ~ [1, 1.06, ... 2.012]
-}


compound : Float -> G.Generator Float Float
compound r =
    G.iterate (\value -> value * r) (1 * r)



{- An example combining income and expense streams with interest rate calculations.

            In each period, some income is received, offset by some expenses. At the end of each period, the remaining balance (net income) is invested at some interest rate and will grow accordingly in the next period.

         For example, if the net income is 20 per period, and the interest rate per period is 3%, assets at the end of each period should be :

      1: 20
      2: (20 * 1.03) + 20 = 40.6
      3: (40.6 * 1.03) + 20 ...

   The below example models cyclical interest rates.
-}


netAssets =
    let
        tally priorAssets ( interestRate, netIncome ) =
            priorAssets * interestRate + netIncome
    in
    G.zipWith (-) income expenses
        |> G.zip interestRates
        |> G.scanl tally 0


income : G.Generator Float ()
income =
    G.repeat 100


expenses : G.Generator Float ()
expenses =
    G.repeat 80


interestRates : G.CycleGenerator Float
interestRates =
    G.cycle [ 1.02, 1.03, 1.04 ]



{- Net present value.

   An example of using the `compound` generator to generate discounted cash flows. The `npv` function collects the cash flows for a finite period and sums them to arrive at the net present value.

-}


npv =
    dcf 1.03 (G.prefix [ 1, 3, 9 ] <| G.repeat 10)
        |> G.take 50
        |> List.foldl (+) 0


dcf : Float -> G.Generator Float b -> G.Generator Float ( Float, b )
dcf discountRate =
    G.zipWith (\rate cash -> cash / rate) (compound discountRate)
