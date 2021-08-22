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
|Data.List|3.524 ns|3.526 ns|3.551 ns|0.004 μs|
|Data.Vector|24.67 ns|86.82 ns|732.4 ns|7.387 μs|
|Data.Vector.Unboxed|14.60 ns|34.57 ns|229.6 ns|2.503 μs|
|Data.Vector.Storable|16.73 ns|43.17 ns|233.3 ns|2.471 μs|
|Data.Sequence|7.135 ns|7.490 ns|7.401 ns|0.007 μs|
|Data.RRBVector|26.61 ns|72.17 ns|72.42 ns|0.078 μs|

## Indexing

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|17.73 ns|146.0 ns|1722 ns|21.13 μs|
|Data.Vector|7.248 ns|7.236 ns|7.218 ns|0.007 μs|
|Data.Vector.Unboxed|7.880 ns|7.880 ns|7.882 ns|0.008 μs|
|Data.Vector.Storable|7.433 ns|7.430 ns|7.427 ns|0.007 μs|
|Data.Sequence|26.05 ns|49.26 ns|76.18 ns|0.028 μs|
|Data.Massiv.Array|10.69 ns|10.65 ns|10.69 ns|0.011 μs|
|Data.RRBVector|16.48 ns|16.42 ns|16.43 ns|0.016 μs|

## Append

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|83.94 ns|826.8 ns|8.677 μs|117.9 μs|
|Data.Vector|42.33 ns|162.7 ns|1.354 μs|14.92 μs|
|Data.Vector.Unboxed|26.02 ns|67.11 ns|0.408 μs|4.449 μs|
|Data.Vector.Storable|26.27 ns|78.33 ns|0.411 μs|4.481 μs|
|Data.Sequence|45.31 ns|98.69 ns|0.147 μs|0.206 μs|
|Data.RRBVector|52.11 ns|679.8 ns|2.617 μs|4.481 μs|

## Length

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|18.24 ns|149.4 ns|1660 ns|20.14 μs|
|Data.Vector|6.506 ns|6.503 ns|6.508 ns|0.007 μs|
|Data.Vector.Unboxed|6.857 ns|6.856 ns|6.857 ns|0.007 μs|
|Data.Vector.Storable|6.308 ns|6.304 ns|6.310 ns|0.006 μs|
|Data.Sequence|6.331 ns|6.333 ns|6.331 ns|0.006 μs|
|Data.Massiv.Array|7.717 ns|7.722 ns|7.727 ns|0.008 μs|
|Data.RRBVector|6.515 ns|6.504 ns|6.517 ns|0.007 μs|

## Stable Sort

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|302.6 ns|9.461 μs|227.5 μs|5.415 ms|
|Data.Vector|145.1 ns|5.254 μs|113.3 μs|2.120 ms|
|Data.Vector.Unboxed|40.80 ns|4.590 μs|93.93 μs|1.506 ms|
|Data.Vector.Storable|41.88 ns|4.130 μs|92.06 μs|1.456 ms|
|Data.Sequence|475.7 ns|9.714 μs|193.5 μs|3.991 ms|

## Replicate

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|55.78 ns|509.7 ns|4.928 μs|49.20 μs|
|Data.Vector|70.74 ns|331.2 ns|2.631 μs|25.46 μs|
|Data.Vector.Unboxed|19.98 ns|40.89 ns|0.281 μs|2.288 μs|
|Data.Vector.Storable|21.39 ns|44.74 ns|0.335 μs|2.975 μs|
|Data.Sequence|58.35 ns|420.3 ns|4.242 μs|42.44 μs|
|Data.RRBVector|53.14 ns|377.3 ns|3.398 μs|33.47 μs|

## Min

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|26.29 ns|235.7 ns|2.231 μs|23.91 μs|
|Data.Vector|23.99 ns|187.2 ns|1.846 μs|18.21 μs|
|Data.Vector.Unboxed|21.11 ns|160.4 ns|1.581 μs|15.61 μs|
|Data.Vector.Storable|15.43 ns|108.0 ns|1.070 μs|10.43 μs|
|Data.Sequence|231.3 ns|2577 ns|29.55 μs|306.6 μs|
|Data.Massiv.Array|22.84 ns|72.91 ns|0.561 μs|5.253 μs|
|Data.RRBVector|88.80 ns|907.5 ns|9.024 μs|89.39 μs|

## Max

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|24.74 ns|241.5 ns|2.234 μs|23.92 μs|
|Data.Vector|23.59 ns|196.1 ns|1.858 μs|18.25 μs|
|Data.Vector.Unboxed|22.85 ns|171.6 ns|1.695 μs|17.98 μs|
|Data.Vector.Storable|14.25 ns|68.73 ns|0.576 μs|5.273 μs|
|Data.Sequence|242.2 ns|2644 ns|29.49 μs|299.2 μs|
|Data.Massiv.Array|25.02 ns|101.9 ns|0.819 μs|7.854 μs|
|Data.RRBVector|88.47 ns|850.3 ns|8.559 μs|84.63 μs|

## Filter Element

|Name|1|100|1000|10000|
|---|---|---|---|---|
|Data.List|91.85 μs|91.81 μs|91.73 μs|91.78 μs|
|Data.Vector|83.48 μs|83.46 μs|83.43 μs|83.55 μs|
|Data.Vector.Unboxed|53.28 μs|53.35 μs|53.32 μs|53.28 μs|
|Data.Vector.Storable|90.45 μs|90.51 μs|90.36 μs|90.44 μs|
|Data.Sequence|233.0 μs|233.4 μs|233.6 μs|257.8 μs|

## Filter By Index

|Name|1|100|1000|10000|
|---|---|---|---|---|
|Data.Vector|148.3 μs|148.2 μs|148.2 μs|148.4 μs|
|Data.Vector.Unboxed|57.77 μs|57.81 μs|57.78 μs|57.63 μs|
|Data.Vector.Storable|88.15 μs|88.14 μs|88.47 μs|89.04 μs|
