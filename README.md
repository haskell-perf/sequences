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
|Data.List|92.68 ns|837.2 ns|9.012 μs|158.7 μs|
|Data.Vector|389.9 ns|8.656 μs|515.0 μs|74.00 ms|
|Data.Vector.Unboxed|323.5 ns|4.012 μs|227.3 μs|24.56 ms|
|Data.Sequence|217.4 ns|2.802 μs|34.63 μs|884.2 μs|

## Replicate

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|119.1 ns|1.003 μs|9.834 μs|98.61 μs|
|Data.Vector|985.3 ns|1.536 μs|4.117 μs|29.28 μs|
|Data.Vector.Unboxed|44.31 ns|107.5 ns|781.1 ns|6.956 μs|
|Data.Sequence|79.31 ns|945.9 ns|9.394 μs|94.60 μs|

## Indexing

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|38.47 ns|199.8 ns|1.923 μs|19.92 μs|
|Data.Vector|36.92 ns|37.01 ns|37.09 ns|36.73 ns|
|Data.Vector.Unboxed|19.84 ns|20.37 ns|20.14 ns|20.13 ns|
|Data.Sequence|41.17 ns|87.40 ns|144.4 ns|48.15 ns|

## Length

|Name|10000|
|---|---|
|Data.List|17.27 μs|
|Data.Vector|18.55 ns|
|Data.Vector.Unboxed|11.06 ns|
|Data.Sequence|10.78 ns|

