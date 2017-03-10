# Containers

For all benchmarks:

    $ stack bench :containers

For specific benchmarks:

    $ stack bench :containers --benchmark-arguments Consing

## Consing

```
benchmarking Consing/Data.List 0..10
time                 95.48 ns   (94.29 ns .. 96.55 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 96.40 ns   (95.19 ns .. 98.21 ns)
std dev              5.079 ns   (3.702 ns .. 7.780 ns)
variance introduced by outliers: 73% (severely inflated)

benchmarking Consing/Data.Vector 0..10
time                 403.1 ns   (399.1 ns .. 406.9 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 404.7 ns   (401.3 ns .. 408.2 ns)
std dev              12.08 ns   (10.32 ns .. 14.51 ns)
variance introduced by outliers: 43% (moderately inflated)

benchmarking Consing/Data.Vector.Unboxed 0..10
time                 320.7 ns   (317.4 ns .. 324.3 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 320.1 ns   (317.0 ns .. 323.6 ns)
std dev              10.91 ns   (9.750 ns .. 12.88 ns)
variance introduced by outliers: 50% (moderately inflated)

benchmarking Consing/Data.Sequence 0..10
time                 204.1 ns   (201.7 ns .. 206.8 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 204.4 ns   (202.0 ns .. 207.4 ns)
std dev              9.285 ns   (7.624 ns .. 11.50 ns)
variance introduced by outliers: 65% (severely inflated)

benchmarking Consing/Data.List 0..1000
time                 9.320 μs   (9.236 μs .. 9.416 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 9.377 μs   (9.257 μs .. 9.524 μs)
std dev              420.8 ns   (338.1 ns .. 570.7 ns)
variance introduced by outliers: 55% (severely inflated)

benchmarking Consing/Data.Vector 0..1000
time                 534.0 μs   (525.1 μs .. 541.1 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 533.0 μs   (527.3 μs .. 540.3 μs)
std dev              22.28 μs   (18.98 μs .. 26.99 μs)
variance introduced by outliers: 35% (moderately inflated)

benchmarking Consing/Data.Vector.Unboxed 0..1000
time                 233.7 μs   (230.9 μs .. 237.5 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 234.9 μs   (231.9 μs .. 238.8 μs)
std dev              11.84 μs   (8.748 μs .. 15.76 μs)
variance introduced by outliers: 48% (moderately inflated)

benchmarking Consing/Data.Sequence 0..1000
time                 35.32 μs   (34.95 μs .. 35.66 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 35.70 μs   (35.25 μs .. 36.38 μs)
std dev              1.834 μs   (1.263 μs .. 3.103 μs)
variance introduced by outliers: 57% (severely inflated)

benchmarking Consing/Data.List 0..10000
time                 164.8 μs   (163.4 μs .. 166.3 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 165.3 μs   (163.6 μs .. 167.2 μs)
std dev              6.240 μs   (5.276 μs .. 7.403 μs)
variance introduced by outliers: 36% (moderately inflated)

benchmarking Consing/Data.Vector 0..10000
time                 77.27 ms   (75.82 ms .. 78.34 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 77.22 ms   (76.84 ms .. 77.82 ms)
std dev              753.6 μs   (460.2 μs .. 1.111 ms)

benchmarking Consing/Data.Vector.Unboxed 0..10000
time                 25.12 ms   (24.49 ms .. 25.72 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 25.35 ms   (25.04 ms .. 25.89 ms)
std dev              890.0 μs   (462.2 μs .. 1.300 ms)

benchmarking Consing/Data.Sequence 0..10000
time                 768.5 μs   (758.9 μs .. 776.1 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 769.0 μs   (761.2 μs .. 779.2 μs)
std dev              30.35 μs   (22.64 μs .. 46.56 μs)
variance introduced by outliers: 30% (moderately inflated)
```

## Replicate

```
benchmarking Replicate/Data.List 0..10
time                 134.8 ns   (133.6 ns .. 136.0 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 135.4 ns   (133.9 ns .. 137.3 ns)
std dev              5.437 ns   (4.415 ns .. 6.692 ns)
variance introduced by outliers: 60% (severely inflated)

benchmarking Replicate/Data.Vector 0..10
time                 940.6 ns   (933.0 ns .. 948.5 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 946.6 ns   (934.9 ns .. 959.5 ns)
std dev              42.02 ns   (34.93 ns .. 50.44 ns)
variance introduced by outliers: 61% (severely inflated)

benchmarking Replicate/Data.Vector.Unboxed 0..10
time                 46.22 ns   (45.66 ns .. 46.76 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 46.60 ns   (46.00 ns .. 47.38 ns)
std dev              2.303 ns   (1.681 ns .. 3.202 ns)
variance introduced by outliers: 71% (severely inflated)

benchmarking Replicate/Data.Sequence 0..10
time                 81.57 ns   (80.81 ns .. 82.46 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 81.88 ns   (81.06 ns .. 82.84 ns)
std dev              2.969 ns   (2.500 ns .. 3.842 ns)
variance introduced by outliers: 56% (severely inflated)

benchmarking Replicate/Data.List 0..1000
time                 10.11 μs   (9.976 μs .. 10.25 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 10.12 μs   (10.01 μs .. 10.27 μs)
std dev              429.2 ns   (345.6 ns .. 525.4 ns)
variance introduced by outliers: 52% (severely inflated)

benchmarking Replicate/Data.Vector 0..1000
time                 4.179 μs   (4.146 μs .. 4.227 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 4.186 μs   (4.148 μs .. 4.231 μs)
std dev              136.7 ns   (114.5 ns .. 164.6 ns)
variance introduced by outliers: 41% (moderately inflated)

benchmarking Replicate/Data.Vector.Unboxed 0..1000
time                 801.9 ns   (793.0 ns .. 813.3 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 803.8 ns   (794.8 ns .. 816.2 ns)
std dev              36.06 ns   (28.43 ns .. 55.22 ns)
variance introduced by outliers: 62% (severely inflated)

benchmarking Replicate/Data.Sequence 0..1000
time                 9.725 μs   (9.631 μs .. 9.825 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 9.749 μs   (9.656 μs .. 9.881 μs)
std dev              376.0 ns   (316.6 ns .. 452.8 ns)
variance introduced by outliers: 48% (moderately inflated)

benchmarking Replicate/Data.List 0..10000
time                 101.8 μs   (100.1 μs .. 103.5 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 101.5 μs   (100.2 μs .. 103.2 μs)
std dev              4.875 μs   (3.820 μs .. 7.209 μs)
variance introduced by outliers: 50% (moderately inflated)

benchmarking Replicate/Data.Vector 0..10000
time                 30.86 μs   (30.44 μs .. 31.27 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 30.92 μs   (30.60 μs .. 31.28 μs)
std dev              1.196 μs   (1.005 μs .. 1.492 μs)
variance introduced by outliers: 44% (moderately inflated)

benchmarking Replicate/Data.Vector.Unboxed 0..10000
time                 7.232 μs   (7.149 μs .. 7.312 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 7.228 μs   (7.155 μs .. 7.329 μs)
std dev              297.3 ns   (234.7 ns .. 399.4 ns)
variance introduced by outliers: 52% (severely inflated)

benchmarking Replicate/Data.Sequence 0..10000
time                 97.21 μs   (96.07 μs .. 98.65 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 97.44 μs   (96.39 μs .. 98.85 μs)
std dev              4.150 μs   (3.562 μs .. 5.121 μs)
variance introduced by outliers: 44% (moderately inflated)
```
