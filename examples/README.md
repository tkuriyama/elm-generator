
# Examples

A variety of examples are provided as sample use cases for generators. The `examples` directory is included in the project `elm.json` `source-directories` list, so they can be imported in `elm-repl` without any qualification.

```elm
% elm-repl

> import Fibonacci
> import Generator as G
> Fibonacci.fib2 |> G.take 5
[0,1,1,2,3]

```



Simple, classic stream functions:

- [Fibonacci](https://github.com/tkuriyama/elm-generator/blob/master/examples/Fibonacci.elm)
- [Collatz](https://github.com/tkuriyama/elm-generator/blob/master/examples/Collatz.elm)

Modeling basic financial math using streams:

- [Finance](https://github.com/tkuriyama/elm-generator/blob/master/examples/Finance.elm)

Extending generator for a domain-specific use case:

- [Timeseries](https://github.com/tkuriyama/elm-generator/blob/master/examples/Timeseries.elm)
