# sequences

Benchmarks for sequence data structures: lists, vectors, etc.

## Running

For all benchmarks:

    $ stack bench :time

For specific benchmarks:

    $ stack bench :time --benchmark-arguments Consing

<!-- RESULTS -->

## Consing

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|10.79 ns|10.86 ns|11.19 ns|0.011 μs|
|Data.Vector|422.6 ns|521.1 ns|1337 ns|10.85 μs|
|Data.Vector.Unboxed|38.44 ns|62.71 ns|431.1 ns|4.789 μs|
|Data.Vector.Storable|28.91 ns|65.31 ns|431.4 ns|4.836 μs|
|Data.Sequence|14.61 ns|15.35 ns|14.71 ns|0.015 μs|

## Indexing

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|34.90 ns|201.9 ns|2008 ns|21.72 μs|
|Data.Vector|35.96 ns|37.65 ns|38.55 ns|0.037 μs|
|Data.Vector.Unboxed|20.75 ns|20.91 ns|21.39 ns|0.021 μs|
|Data.Vector.Storable|16.41 ns|16.49 ns|16.90 ns|0.017 μs|
|Data.Sequence|38.10 ns|72.18 ns|124.4 ns|0.038 μs|

## Append

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|199.1 ns|1514 ns|15.91 μs|256.7 μs|
|Data.Vector|500.7 ns|649.2 ns|2.204 μs|21.45 μs|
|Data.Vector.Unboxed|55.60 ns|113.3 ns|0.827 μs|9.474 μs|
|Data.Vector.Storable|43.61 ns|120.2 ns|0.824 μs|9.320 μs|
|Data.Sequence|79.84 ns|175.5 ns|0.301 μs|0.396 μs|

## Length

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|28.38 ns|192.0 ns|1986 ns|23.03 μs|
|Data.Vector|19.19 ns|20.19 ns|19.68 ns|0.020 μs|
|Data.Vector.Unboxed|12.62 ns|12.98 ns|12.96 ns|0.013 μs|
|Data.Vector.Storable|11.44 ns|11.86 ns|12.06 ns|0.012 μs|
|Data.Sequence|10.50 ns|11.00 ns|10.73 ns|0.011 μs|

## Stable Sort

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|0.810 μs|15.35 μs|293.4 μs|9.692 ms|
|Data.Vector|1.072 μs|11.28 μs|151.1 μs|2.686 ms|
|Data.Vector.Unboxed|0.951 μs|8.194 μs|88.66 μs|1.260 ms|
|Data.Vector.Storable|0.924 μs|7.375 μs|80.36 μs|1.117 ms|
|Data.Sequence|2.815 μs|29.98 μs|504.7 μs|13.53 ms|

## Replicate

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|116.0 ns|1072 ns|9.957 μs|101.1 μs|
|Data.Vector|1033 ns|1757 ns|4.659 μs|31.19 μs|
|Data.Vector.Unboxed|46.37 ns|110.9 ns|0.799 μs|7.209 μs|
|Data.Vector.Storable|32.03 ns|104.8 ns|0.805 μs|7.222 μs|
|Data.Sequence|110.6 ns|1039 ns|10.42 μs|102.8 μs|

## Min

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|46.53 ns|350.6 ns|3.390 μs|33.91 μs|
|Data.Vector|38.80 ns|231.4 ns|2.030 μs|19.26 μs|
|Data.Vector.Unboxed|27.17 ns|133.2 ns|0.933 μs|8.792 μs|
|Data.Vector.Storable|28.20 ns|142.5 ns|0.980 μs|8.818 μs|

## Max

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|35.84 ns|348.5 ns|3.331 μs|35.24 μs|
|Data.Vector|34.03 ns|232.7 ns|1.940 μs|20.24 μs|
|Data.Vector.Unboxed|23.07 ns|108.0 ns|0.886 μs|8.817 μs|
|Data.Vector.Storable|19.59 ns|104.5 ns|0.929 μs|8.762 μs|

## Filter Element

|Name|1|100|1000|10000|
|---|---|---|---|---|
|Data.List|182.3 μs|174.3 μs|175.0 μs|182.4 μs|
|Data.Vector|780.7 μs|758.4 μs|761.7 μs|758.0 μs|
|Data.Vector.Unboxed|83.35 μs|82.07 μs|84.79 μs|87.49 μs|
|Data.Vector.Storable|142.1 μs|142.0 μs|149.3 μs|149.2 μs|
|Data.Sequence|727.8 μs|734.5 μs|711.8 μs|700.9 μs|

## Filter By Index

|Name|1|100|1000|10000|
|---|---|---|---|---|
|Data.Vector|751.3 μs|758.5 μs|764.5 μs|734.0 μs|
|Data.Vector.Unboxed|86.76 μs|87.42 μs|85.25 μs|85.56 μs|
|Data.Vector.Storable|148.3 μs|156.5 μs|149.2 μs|142.6 μs|

