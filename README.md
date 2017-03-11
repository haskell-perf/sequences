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
|Data.List:10 | 92.68 ns | 91.74 ns | 93.71 ns | 3.223 ns|
|Data.Vector:10 | 389.9 ns | 386.2 ns | 395.2 ns | 14.78 ns|
|Data.Vector.Unboxed:10 | 323.5 ns | 320.1 ns | 327.2 ns | 11.76 ns|
|Data.Sequence:10 | 217.4 ns | 215.9 ns | 219.8 ns | 6.418 ns|
|Data.List:100 | 837.2 ns | 830.5 ns | 848.0 ns | 27.07 ns|
|Data.Vector:100 | 8.656 μs | 8.568 μs | 8.789 μs | 360.9 ns|
|Data.Vector.Unboxed:100 | 4.012 μs | 3.983 μs | 4.052 μs | 110.7 ns|
|Data.Sequence:100 | 2.802 μs | 2.779 μs | 2.831 μs | 86.26 ns|
|Data.List:1000 | 9.012 μs | 8.955 μs | 9.100 μs | 236.6 ns|
|Data.Vector:1000 | 515.0 μs | 511.5 μs | 520.5 μs | 14.81 μs|
|Data.Vector.Unboxed:1000 | 227.3 μs | 225.9 μs | 229.0 μs | 5.039 μs|
|Data.Sequence:1000 | 34.63 μs | 34.28 μs | 35.03 μs | 1.251 μs|
|Data.List:10000 | 158.7 μs | 157.7 μs | 160.6 μs | 4.451 μs|
|Data.Vector:10000 | 74.00 ms | 73.17 ms | 74.68 ms | 1.299 ms|
|Data.Vector.Unboxed:10000 | 24.56 ms | 24.39 ms | 24.77 ms | 420.4 μs|
|Data.Sequence:10000 | 884.2 μs | 876.9 μs | 893.7 μs | 29.59 μs|

## Replicate

|Name|Mean|Min|Max|Stddev|
|---|---|---|---|---|
|Data.List:10 | 119.1 ns | 118.1 ns | 120.7 ns | 4.083 ns|
|Data.Vector:10 | 985.3 ns | 977.0 ns | 998.5 ns | 35.93 ns|
|Data.Vector.Unboxed:10 | 44.31 ns | 43.98 ns | 44.85 ns | 1.329 ns|
|Data.Sequence:10 | 79.31 ns | 78.64 ns | 80.13 ns | 2.485 ns|
|Data.List:100 | 1.003 μs | 994.9 ns | 1.014 μs | 30.80 ns|
|Data.Vector:100 | 1.536 μs | 1.525 μs | 1.556 μs | 46.92 ns|
|Data.Vector.Unboxed:100 | 107.5 ns | 107.0 ns | 108.3 ns | 2.226 ns|
|Data.Sequence:100 | 945.9 ns | 941.2 ns | 951.6 ns | 17.09 ns|
|Data.List:1000 | 9.834 μs | 9.760 μs | 9.948 μs | 295.0 ns|
|Data.Vector:1000 | 4.117 μs | 4.096 μs | 4.159 μs | 97.66 ns|
|Data.Vector.Unboxed:1000 | 781.1 ns | 775.2 ns | 790.7 ns | 25.19 ns|
|Data.Sequence:1000 | 9.394 μs | 9.326 μs | 9.503 μs | 285.1 ns|
|Data.List:10000 | 98.61 μs | 97.76 μs | 99.90 μs | 3.610 μs|
|Data.Vector:10000 | 29.28 μs | 29.09 μs | 29.62 μs | 833.7 ns|
|Data.Vector.Unboxed:10000 | 6.956 μs | 6.918 μs | 7.028 μs | 161.9 ns|
|Data.Sequence:10000 | 94.60 μs | 93.83 μs | 95.50 μs | 2.741 μs|

## Indexing

|Name|Mean|Min|Max|Stddev|
|---|---|---|---|---|
|Data.List:10 | 38.47 ns | 38.16 ns | 38.98 ns | 1.299 ns|
|Data.Vector:10 | 36.92 ns | 36.57 ns | 37.49 ns | 1.415 ns|
|Data.Vector.Unboxed:10 | 19.84 ns | 19.71 ns | 20.04 ns | 567.8 ps|
|Data.Sequence:10 | 41.17 ns | 40.75 ns | 41.64 ns | 1.498 ns|
|Data.List:100 | 199.8 ns | 198.5 ns | 201.8 ns | 5.173 ns|
|Data.Vector:100 | 37.01 ns | 36.73 ns | 37.45 ns | 1.169 ns|
|Data.Vector.Unboxed:100 | 20.37 ns | 20.24 ns | 20.54 ns | 485.2 ps|
|Data.Sequence:100 | 87.40 ns | 86.82 ns | 88.41 ns | 2.383 ns|
|Data.List:1000 | 1.923 μs | 1.911 μs | 1.941 μs | 49.21 ns|
|Data.Vector:1000 | 37.09 ns | 36.83 ns | 37.42 ns | 976.4 ps|
|Data.Vector.Unboxed:1000 | 20.14 ns | 19.99 ns | 20.36 ns | 581.3 ps|
|Data.Sequence:1000 | 144.4 ns | 143.4 ns | 145.8 ns | 3.968 ns|
|Data.List:10000 | 19.92 μs | 19.77 μs | 20.13 μs | 597.2 ns|
|Data.Vector:10000 | 36.73 ns | 36.54 ns | 37.21 ns | 924.7 ps|
|Data.Vector.Unboxed:10000 | 20.13 ns | 19.99 ns | 20.37 ns | 577.8 ps|
|Data.Sequence:10000 | 48.15 ns | 47.84 ns | 48.66 ns | 1.346 ns|

## Length

|Name|Mean|Min|Max|Stddev|
|---|---|---|---|---|
|Data.List:10000 | 17.27 μs | 17.11 μs | 17.46 μs | 573.8 ns|
|Data.Vector:10000 | 18.55 ns | 18.45 ns | 18.67 ns | 370.7 ps|
|Data.Vector.Unboxed:10000 | 11.06 ns | 10.97 ns | 11.17 ns | 351.1 ps|
|Data.Sequence:10000 | 10.78 ns | 10.70 ns | 10.90 ns | 330.8 ps|
