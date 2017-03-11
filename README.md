# Sequences

In computer science, a list or sequence is an abstract data type
that represents a countable number of ordered values, where the same
value may occur more than once. An instance of a list is a computer
representation of the mathematical concept of a finite sequence.

From [Wikipedia](https://en.wikipedia.org/wiki/List_(abstract_data_type)).

## Running

For all benchmarks:

    $ stack bench :time

For specific benchmarks:

    $ stack bench :time --benchmark-arguments Consing

<!-- RESULTS -->

## Consing

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|92.68 ns|0.837 μs|9.012 μs|0.159 ms|
|Data.Vector|389.9 ns|8.656 μs|515.0 μs|74.00 ms|
|Data.Vector.Unboxed|323.5 ns|4.012 μs|227.3 μs|24.56 ms|
|Data.Sequence|217.4 ns|2.802 μs|34.63 μs|0.884 ms|

## Replicate

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|119.1 ns|1003 ns|9.834 μs|98.61 μs|
|Data.Vector|985.3 ns|1536 ns|4.117 μs|29.28 μs|
|Data.Vector.Unboxed|44.31 ns|107.5 ns|0.781 μs|6.956 μs|
|Data.Sequence|79.31 ns|945.9 ns|9.394 μs|94.60 μs|

## Indexing

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|38.47 ns|199.8 ns|1923 ns|19.92 μs|
|Data.Vector|36.92 ns|37.01 ns|37.09 ns|0.037 μs|
|Data.Vector.Unboxed|19.84 ns|20.37 ns|20.14 ns|0.020 μs|
|Data.Sequence|41.17 ns|87.40 ns|144.4 ns|0.048 μs|

## Length

|Name|10000|
|---|---|
|Data.List|17.27 μs|
|Data.Vector|0.019 μs|
|Data.Vector.Unboxed|0.011 μs|
|Data.Sequence|0.011 μs|

