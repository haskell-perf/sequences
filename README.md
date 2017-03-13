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
|Data.List|11.14 ns|11.42 ns|11.34 ns|0.012 μs|
|Data.Vector|440.5 ns|521.0 ns|1306 ns|11.55 μs|
|Data.Vector.Unboxed|37.74 ns|65.38 ns|434.1 ns|4.932 μs|
|Data.Vector.Storable|27.82 ns|64.74 ns|422.2 ns|4.894 μs|
|Data.Sequence|14.92 ns|15.65 ns|14.69 ns|0.015 μs|

## Indexing

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|36.01 ns|202.0 ns|1997 ns|22.08 μs|
|Data.Vector|38.30 ns|36.77 ns|36.33 ns|0.037 μs|
|Data.Vector.Unboxed|21.59 ns|21.63 ns|21.26 ns|0.021 μs|
|Data.Vector.Storable|16.60 ns|16.62 ns|16.74 ns|0.017 μs|
|Data.Sequence|37.92 ns|70.83 ns|120.2 ns|0.038 μs|

## Append

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|189.2 ns|1524 ns|15.69 μs|255.3 μs|
|Data.Vector|505.3 ns|674.0 ns|2.226 μs|21.22 μs|
|Data.Vector.Unboxed|53.56 ns|114.3 ns|0.835 μs|9.379 μs|
|Data.Vector.Storable|43.10 ns|120.4 ns|0.835 μs|9.447 μs|
|Data.Sequence|78.98 ns|177.7 ns|0.302 μs|0.409 μs|

## Length

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|28.89 ns|193.0 ns|1880 ns|22.96 μs|
|Data.Vector|19.95 ns|18.93 ns|19.08 ns|0.019 μs|
|Data.Vector.Unboxed|13.50 ns|12.61 ns|12.51 ns|0.012 μs|
|Data.Vector.Storable|12.18 ns|11.73 ns|11.37 ns|0.011 μs|
|Data.Sequence|10.62 ns|10.58 ns|10.81 ns|0.011 μs|

## Stable Sort

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|0.814 μs|16.13 μs|302.2 μs|9.853 ms|
|Data.Vector|1.107 μs|11.74 μs|166.4 μs|2.794 ms|
|Data.Vector.Unboxed|0.991 μs|8.418 μs|93.78 μs|1.238 ms|
|Data.Vector.Storable|0.903 μs|7.470 μs|81.11 μs|1.113 ms|
|Data.Sequence|2.827 μs|31.07 μs|524.0 μs|14.17 ms|

## Replicate

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|122.4 ns|1086 ns|10.60 μs|104.8 μs|
|Data.Vector|984.0 ns|1688 ns|4.803 μs|32.86 μs|
|Data.Vector.Unboxed|43.94 ns|115.0 ns|0.841 μs|7.534 μs|
|Data.Vector.Storable|31.11 ns|111.4 ns|0.824 μs|7.470 μs|
|Data.Sequence|103.7 ns|1097 ns|10.79 μs|107.4 μs|

## Min

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|48.03 ns|352.5 ns|3.412 μs|34.84 μs|
|Data.Vector|39.09 ns|240.5 ns|2.037 μs|20.24 μs|
|Data.Vector.Unboxed|26.39 ns|133.2 ns|0.938 μs|9.140 μs|
|Data.Vector.Storable|27.40 ns|142.9 ns|0.980 μs|9.106 μs|

## Max

|Name|10|100|1000|10000|
|---|---|---|---|---|
|Data.List|36.90 ns|367.9 ns|3.391 μs|34.58 μs|
|Data.Vector|34.94 ns|235.5 ns|2.053 μs|19.86 μs|
|Data.Vector.Unboxed|22.77 ns|109.1 ns|0.914 μs|8.901 μs|
|Data.Vector.Storable|20.36 ns|106.4 ns|0.923 μs|8.849 μs|

## Filter Element

|Name|1|100|1000|10000|
|---|---|---|---|---|
|Data.List|183.1 μs|183.4 μs|185.2 μs|187.8 μs|
|Data.Vector|787.1 μs|790.8 μs|799.3 μs|812.4 μs|
|Data.Vector.Unboxed|85.76 μs|85.61 μs|87.06 μs|84.19 μs|
|Data.Vector.Storable|148.2 μs|148.1 μs|148.5 μs|147.7 μs|
|Data.Sequence|729.8 μs|750.7 μs|753.5 μs|754.6 μs|

## Filter By Index

|Name|1|100|1000|10000|
|---|---|---|---|---|
|Data.Vector|799.8 μs|773.1 μs|798.6 μs|770.4 μs|
|Data.Vector.Unboxed|91.60 μs|93.09 μs|91.41 μs|89.76 μs|
|Data.Vector.Storable|159.6 μs|162.0 μs|156.6 μs|150.1 μs|

