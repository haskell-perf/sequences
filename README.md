# sequences

Benchmarks for sequence data structures: lists, vectors, etc.

## Running

For all benchmarks:

    $ stack bench :time

For specific benchmarks:

    $ stack bench :time --benchmark-arguments Consing

<!-- RESULTS -->

## Append

|Name|10|100|1000|10000|
|---|---|---|---|---|---|---|---|
|Data.List|173.9 ns|1.370 μs|14.15 μs|134.2 μs|
|Data.Vector|584.0 ns|1.034 μs|5.910 μs|57.72 μs|
|Data.Vector.Unboxed|54.21 ns|0.110 μs|0.807 μs|9.077 μs|
|Data.Sequence|242.9 ns|1.821 μs|15.71 μs|150.4 μs|

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

## Min

|Name|10000|
|---|---|
|Data.List|154.0 μs|
|Data.Vector|22.92 μs|
|Data.Vector.Unboxed|6.631 μs|

## Max

|Name|10000|
|---|---|
|Data.List|145.1 μs|
|Data.Vector|23.85 μs|
|Data.Vector.Unboxed|6.522 μs|

## Sort

|Name|10000|
|---|---|
|Data.List|0.296 ms|
|Data.Sequence|3.959 ms|

## Remove Element

|Name|1|100|1000|10000|
|---|---|---|---|---|
|Data.List|156.4 μs|176.0 μs|171.0 μs|171.4 μs|
|Data.Vector|744.4 μs|737.9 μs|745.0 μs|743.8 μs|
|Data.Vector.Unboxed|64.00 μs|77.40 μs|76.44 μs|77.84 μs|
|Data.Sequence|2240 μs|2246 μs|2229 μs|2252 μs|

## Remove By Index

|Name|1|100|1000|10000|
|---|---|---|---|---|
|Data.Vector|737.1 μs|744.0 μs|734.6 μs|743.2 μs|
|Data.Vector.Unboxed|68.41 μs|82.31 μs|81.93 μs|82.13 μs|
