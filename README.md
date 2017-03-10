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

|Name|Mean|Min|Max|Stddev|
|---|---|---|---|---|
|Data.List 0..10 | 103.5 ns | 102.4 ns | 104.7 ns | 3.884 ns|
|Data.Vector 0..10 | 402.0 ns | 398.3 ns | 408.7 ns | 16.36 ns|
|Data.Vector.Unboxed 0..10 | 320.8 ns | 317.5 ns | 325.5 ns | 13.18 ns|
|Data.Sequence 0..10 | 203.5 ns | 202.0 ns | 205.2 ns | 5.232 ns|
|Data.List 0..1000 | 11.90 μs | 11.80 μs | 12.02 μs | 366.8 ns|
|Data.Vector 0..1000 | 516.3 μs | 513.0 μs | 521.0 μs | 13.07 μs|
|Data.Vector.Unboxed 0..1000 | 228.3 μs | 226.5 μs | 231.0 μs | 7.259 μs|
|Data.Sequence 0..1000 | 32.76 μs | 32.56 μs | 33.05 μs | 792.3 ns|
|Data.List 0..10000 | 186.4 μs | 185.1 μs | 188.7 μs | 5.481 μs|
|Data.Vector 0..10000 | 73.65 ms | 73.06 ms | 74.40 ms | 1.061 ms|
|Data.Vector.Unboxed 0..10000 | 24.57 ms | 24.43 ms | 24.77 ms | 360.7 μs|
|Data.Sequence 0..10000 | 860.6 μs | 853.5 μs | 872.4 μs | 29.57 μs|

## Replicate

|Name|Mean|Min|Max|Stddev|
|---|---|---|---|---|
|Data.List 10 | 120.4 ns | 119.3 ns | 121.9 ns | 4.463 ns|
|Data.Vector 10 | 935.3 ns | 928.5 ns | 944.0 ns | 26.66 ns|
|Data.Vector.Unboxed 10 | 44.77 ns | 44.47 ns | 45.13 ns | 1.188 ns|
|Data.Sequence 10 | 94.03 ns | 93.34 ns | 95.27 ns | 3.067 ns|
|Data.List 1000 | 9.817 μs | 9.748 μs | 9.907 μs | 266.8 ns|
|Data.Vector 1000 | 4.110 μs | 4.081 μs | 4.154 μs | 115.3 ns|
|Data.Vector.Unboxed 1000 | 779.5 ns | 773.7 ns | 788.9 ns | 24.54 ns|
|Data.Sequence 1000 | 8.168 μs | 8.111 μs | 8.252 μs | 216.3 ns|
|Data.List 10000 | 96.68 μs | 96.25 μs | 97.27 μs | 1.604 μs|
|Data.Vector 10000 | 29.80 μs | 29.54 μs | 30.15 μs | 1.017 μs|
|Data.Vector.Unboxed 10000 | 6.969 μs | 6.921 μs | 7.050 μs | 202.3 ns|
|Data.Sequence 10000 | 80.71 μs | 79.95 μs | 81.61 μs | 2.723 μs|

## Indexing

|Name|Mean|Min|Max|Stddev|
|---|---|---|---|---|
|Data.List 100 | 193.7 ns | 192.6 ns | 195.7 ns | 4.956 ns|
|Data.Vector 100 | 39.41 ns | 39.17 ns | 39.73 ns | 935.8 ps|
|Data.Vector.Unboxed 100 | 19.90 ns | 19.77 ns | 20.09 ns | 511.4 ps|
|Data.Sequence 100 | 83.17 ns | 82.69 ns | 83.88 ns | 1.875 ns|
|Data.List 1000 | 1.897 μs | 1.882 μs | 1.916 μs | 55.45 ns|
|Data.Vector 1000 | 39.16 ns | 38.91 ns | 39.57 ns | 1.014 ns|
|Data.Vector.Unboxed 1000 | 19.76 ns | 19.58 ns | 19.99 ns | 696.1 ps|
|Data.Sequence 1000 | 139.3 ns | 138.2 ns | 140.7 ns | 3.957 ns|
|Data.List 8000 | 14.96 μs | 14.86 μs | 15.11 μs | 394.3 ns|
|Data.Vector 8000 | 39.60 ns | 39.26 ns | 40.09 ns | 1.299 ns|
|Data.Vector.Unboxed 8000 | 19.53 ns | 19.41 ns | 19.71 ns | 468.2 ps|
|Data.Sequence 8000 | 166.4 ns | 165.2 ns | 168.4 ns | 5.219 ns|
