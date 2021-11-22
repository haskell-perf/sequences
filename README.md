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
|Data.List|3.457 ns|4.099 ns|3.425 ns|0.003 μs|
|Data.DList|5.757 ns|5.268 ns|5.568 ns|0.005 μs|
|Data.Acc|5.601 ns|6.143 ns|5.883 ns|0.006 μs|
|Data.Sequence|7.249 ns|9.292 ns|7.571 ns|0.008 μs|
|Data.RRBVector|23.07 ns|60.90 ns|63.95 ns|0.070 μs|
|Data.Vector.Storable|16.10 ns|44.17 ns|236.0 ns|2.487 μs|
|Data.Vector.Unboxed|14.35 ns|58.29 ns|233.6 ns|2.504 μs|
|Data.Vector|34.39 ns|93.55 ns|749.3 ns|7.583 μs|

## Snocing

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.DList|5.532 ns|5.019 ns|6.397 ns|0.006 μs|
|Data.Acc|5.913 ns|7.140 ns|8.277 ns|0.006 μs|
|Data.Sequence|7.285 ns|7.183 ns|7.224 ns|0.007 μs|
|Data.RRBVector|22.92 ns|60.99 ns|54.70 ns|0.055 μs|
|Data.Vector.Unboxed|15.75 ns|45.87 ns|236.3 ns|2.458 μs|
|Data.Vector.Storable|15.35 ns|50.05 ns|309.1 ns|2.401 μs|
|Data.Vector|27.68 ns|88.50 ns|741.3 ns|8.582 μs|

## Indexing

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Vector.Storable|6.634 ns|7.380 ns|6.404 ns|0.006 μs|
|Data.Vector|6.851 ns|9.825 ns|12.79 ns|0.007 μs|
|Data.Vector.Unboxed|7.235 ns|8.447 ns|15.47 ns|0.008 μs|
|Data.Massiv.Array|11.08 ns|14.58 ns|9.754 ns|0.009 μs|
|Data.RRBVector|16.67 ns|23.26 ns|13.57 ns|0.014 μs|
|Data.Sequence|25.14 ns|48.31 ns|66.50 ns|0.024 μs|
|Data.List|17.55 ns|213.3 ns|3577 ns|27.82 μs|

## Append

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.DList|8.959 ns|8.967 ns|0.022 μs|0.015 μs|
|Data.Acc|9.058 ns|13.44 ns|0.021 μs|0.017 μs|
|Data.Sequence|44.08 ns|102.0 ns|0.234 μs|0.414 μs|
|Data.Vector.Unboxed|24.02 ns|69.99 ns|0.571 μs|6.627 μs|
|Data.Vector.Storable|24.38 ns|79.54 ns|0.580 μs|6.706 μs|
|Data.RRBVector|45.98 ns|705.1 ns|3.848 μs|6.971 μs|
|Data.Vector|40.22 ns|164.6 ns|1.993 μs|23.25 μs|
|Data.List|72.52 ns|781.1 ns|15.43 μs|192.0 μs|

## Length

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Vector.Storable|8.156 ns|8.277 ns|0.009 μs|0.009 μs|
|Data.Sequence|9.241 ns|8.856 ns|0.010 μs|0.009 μs|
|Data.RRBVector|9.678 ns|9.383 ns|0.009 μs|0.010 μs|
|Data.Vector|10.54 ns|9.682 ns|0.010 μs|0.010 μs|
|Data.Vector.Unboxed|10.72 ns|9.999 ns|0.010 μs|0.010 μs|
|Data.Massiv.Array|10.46 ns|10.20 ns|0.010 μs|0.011 μs|
|Data.List|21.64 ns|185.4 ns|2.197 μs|27.09 μs|
|Data.DList|132.4 ns|956.7 ns|9.282 μs|95.80 μs|
|Data.Acc|122.5 ns|1295 ns|13.69 μs|190.7 μs|

## Stable Sort

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Vector.Unboxed|85.88 ns|7.150 μs|136.4 μs|2.360 ms|
|Data.Vector.Storable|73.52 ns|6.466 μs|142.7 μs|2.445 ms|
|Data.Vector|306.2 ns|8.012 μs|169.8 μs|3.241 ms|
|Data.Sequence|980.3 ns|14.55 μs|295.1 μs|7.430 ms|
|Data.List|502.1 ns|15.58 μs|288.9 μs|9.445 ms|

## Replicate

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Vector.Unboxed|32.34 ns|96.80 ns|0.532 μs|3.522 μs|
|Data.Vector.Storable|36.31 ns|110.2 ns|0.474 μs|4.451 μs|
|Data.RRBVector|263.3 ns|460.7 ns|3.015 μs|33.00 μs|
|Data.Vector|87.69 ns|739.2 ns|3.742 μs|36.52 μs|
|Data.Sequence|93.59 ns|731.2 ns|5.813 μs|56.32 μs|
|Data.List|114.2 ns|2300 ns|9.543 μs|80.01 μs|
|Data.DList|163.2 ns|2108 ns|10.40 μs|92.43 μs|

## Min

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Vector.Storable|16.02 ns|0.081 μs|0.769 μs|8.157 μs|
|Data.Massiv.Array|39.87 ns|0.090 μs|0.782 μs|8.697 μs|
|Data.Vector.Unboxed|17.54 ns|0.096 μs|0.842 μs|9.815 μs|
|Data.Vector|26.36 ns|0.202 μs|2.086 μs|21.78 μs|
|Data.List|26.50 ns|0.290 μs|2.903 μs|29.08 μs|
|Data.RRBVector|111.4 ns|1.095 μs|12.00 μs|122.6 μs|
|Data.DList|231.5 ns|2.109 μs|25.42 μs|384.9 μs|
|Data.Sequence|385.4 ns|3.662 μs|49.03 μs|476.3 μs|
|Data.Acc|383.4 ns|3.811 μs|45.25 μs|813.3 μs|

## Max

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.Vector.Storable|16.40 ns|0.102 μs|0.771 μs|7.069 μs|
|Data.Massiv.Array|39.29 ns|0.105 μs|0.793 μs|7.955 μs|
|Data.Vector.Unboxed|24.51 ns|0.132 μs|1.126 μs|11.23 μs|
|Data.Vector|29.32 ns|0.208 μs|1.919 μs|18.79 μs|
|Data.List|30.56 ns|0.341 μs|2.867 μs|29.62 μs|
|Data.RRBVector|110.4 ns|1.111 μs|11.29 μs|132.4 μs|
|Data.DList|256.9 ns|4.532 μs|17.41 μs|246.5 μs|
|Data.Sequence|275.2 ns|3.133 μs|43.81 μs|449.9 μs|
|Data.Acc|353.5 ns|3.778 μs|40.18 μs|977.2 μs|

## Filter Element

|Name|1|100|1000|10000|
|---|---|---|---|---|
|Data.Vector.Unboxed|68.01 μs|58.17 μs|70.60 μs|56.90 μs|
|Data.Vector|101.1 μs|90.79 μs|86.63 μs|87.79 μs|
|Data.Vector.Storable|119.4 μs|100.7 μs|106.2 μs|95.62 μs|
|Data.List|102.6 μs|148.6 μs|148.8 μs|99.55 μs|
|Data.Sequence|436.2 μs|315.9 μs|265.4 μs|298.9 μs|

## Filter By Index

|Name|1|100|1000|10000|
|---|---|---|---|---|
|Data.Vector.Unboxed|70.23 μs|119.1 μs|95.71 μs|72.42 μs|
|Data.Vector.Storable|122.9 μs|166.2 μs|114.3 μs|130.1 μs|
|Data.Vector|188.4 μs|179.2 μs|342.2 μs|222.0 μs|

