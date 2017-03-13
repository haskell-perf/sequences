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
|Data.List|9.799 ns|9.962 ns|10.03 ns|0.010 μs|
|Data.Vector|497.4 ns|505.0 ns|1384 ns|10.70 μs|
|Data.Vector.Unboxed|41.57 ns|64.19 ns|427.0 ns|4.692 μs|
|Data.Vector.Storable|31.62 ns|67.64 ns|425.7 ns|4.740 μs|
|Data.Sequence|15.88 ns|16.14 ns|16.17 ns|0.016 μs|

## Indexing

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|35.72 ns|198.0 ns|1821 ns|20.26 μs|
|Data.Vector|37.92 ns|37.77 ns|38.55 ns|0.038 μs|
|Data.Vector.Unboxed|20.07 ns|19.70 ns|19.78 ns|0.020 μs|
|Data.Vector.Storable|15.54 ns|15.18 ns|15.10 ns|0.015 μs|
|Data.Sequence|41.75 ns|86.69 ns|143.3 ns|0.049 μs|

## Append

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|193.0 ns|1727 ns|17.04 μs|170.6 μs|
|Data.Vector|557.9 ns|999.2 ns|5.805 μs|64.55 μs|
|Data.Vector.Unboxed|56.35 ns|114.2 ns|0.806 μs|8.912 μs|
|Data.Vector.Storable|46.11 ns|119.9 ns|0.807 μs|9.065 μs|
|Data.Sequence|248.4 ns|1901 ns|16.67 μs|160.6 μs|

## Length

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|26.24 ns|175.3 ns|1708 ns|21.28 μs|
|Data.Vector|19.71 ns|19.94 ns|19.93 ns|0.020 μs|
|Data.Vector.Unboxed|11.59 ns|11.85 ns|11.84 ns|0.012 μs|
|Data.Vector.Storable|10.53 ns|10.83 ns|10.80 ns|0.011 μs|
|Data.Sequence|11.02 ns|11.32 ns|11.27 ns|0.011 μs|

## Stable Sort

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|0.789 μs|14.62 μs|276.8 μs|9.438 ms|
|Data.Vector|1.063 μs|11.20 μs|150.9 μs|2.690 ms|
|Data.Vector.Unboxed|0.916 μs|7.683 μs|89.95 μs|1.194 ms|
|Data.Vector.Storable|0.890 μs|7.479 μs|86.09 μs|1.081 ms|
|Data.Sequence|2.734 μs|28.69 μs|498.4 μs|13.73 ms|

## Replicate

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|124.5 ns|1056 ns|10.43 μs|103.0 μs|
|Data.Vector|1015 ns|1629 ns|4.257 μs|30.30 μs|
|Data.Vector.Unboxed|44.80 ns|108.9 ns|0.772 μs|6.981 μs|
|Data.Vector.Storable|35.39 ns|106.1 ns|0.768 μs|7.006 μs|
|Data.Sequence|84.09 ns|899.0 ns|8.201 μs|80.58 μs|

## Min

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|158.0 ns|1505 ns|14.91 μs|149.4 μs|
|Data.Vector|35.70 ns|221.2 ns|1.907 μs|18.96 μs|
|Data.Vector.Unboxed|24.94 ns|128.7 ns|0.911 μs|8.584 μs|
|Data.Vector.Storable|23.96 ns|123.8 ns|0.914 μs|8.528 μs|

## Max

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|147.4 ns|1345 ns|13.23 μs|131.8 μs|
|Data.Vector|30.90 ns|222.9 ns|1.887 μs|18.89 μs|
|Data.Vector.Unboxed|21.18 ns|101.2 ns|0.871 μs|8.469 μs|
|Data.Vector.Storable|17.70 ns|99.93 ns|0.873 μs|8.515 μs|

## Filter Element

|Name|1|100|1000|10000|
|---|---|---|---|---|
|Data.List|161.2 μs|173.0 μs|172.8 μs|174.3 μs|
|Data.Vector|745.2 μs|752.6 μs|758.0 μs|760.9 μs|
|Data.Vector.Unboxed|66.89 μs|78.06 μs|78.09 μs|78.17 μs|
|Data.Vector.Storable|115.5 μs|125.8 μs|126.7 μs|127.6 μs|
|Data.Sequence|2436 μs|2433 μs|2440 μs|2453 μs|

## Filter By Index

|Name|1|100|1000|10000|
|---|---|---|---|---|
|Data.Vector|748.1 μs|766.6 μs|755.4 μs|770.6 μs|
|Data.Vector.Unboxed|68.51 μs|86.80 μs|82.48 μs|86.51 μs|
|Data.Vector.Storable|132.8 μs|147.9 μs|143.5 μs|149.6 μs|

