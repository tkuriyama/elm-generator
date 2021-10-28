
# Examples

A variety of examples are provided as sample use cases for generators.

```elm
% elm-repl

> import Examples.Fibonacci as Fibonacci
> import Generator as G
> Fibonacci.fib2 |> G.take 5
[0,1,1,2,3]

```



Simple, classic stream functions:

- [Fibonacci](https://github.com/tkuriyama/elm-generator/blob/master/src/Examples/Fibonacci.elm)
- [Collatz](https://github.com/tkuriyama/elm-generator/blob/master/src/Examples/Collatz.elm)

Prime numbers, including an implementation of the Sieve of Eratosthenes:

- [Primes](https://github.com/tkuriyama/elm-generator/blob/master/src/Examples/Primes.elm)

Modeling basic financial math using streams:

- [Finance](https://github.com/tkuriyama/elm-generator/blob/master/src/Examples/Finance.elm)

Extending generator for a domain-specific use case:

- [Timeseries](https://github.com/tkuriyama/elm-generator/blob/master/src/Examples/Timeseries.elm)
