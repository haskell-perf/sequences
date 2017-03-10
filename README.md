# Containers

For all benchmarks:

    $ stack bench :containers

For specific benchmarks:

    $ stack bench :containers --benchmark-arguments Consing

## Results

```
benchmarking Consing/Data.List 0..10
time                 93.47 ns   (92.87 ns .. 94.13 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 93.16 ns   (92.60 ns .. 93.87 ns)
std dev              2.082 ns   (1.647 ns .. 2.858 ns)
variance introduced by outliers: 32% (moderately inflated)

benchmarking Consing/Data.Vector 0..10
time                 392.2 ns   (388.7 ns .. 396.4 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 398.0 ns   (393.5 ns .. 404.6 ns)
std dev              18.63 ns   (13.55 ns .. 26.47 ns)
variance introduced by outliers: 65% (severely inflated)

benchmarking Consing/Data.Vector.Unboxed 0..10
time                 327.3 ns   (323.1 ns .. 333.6 ns)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 331.7 ns   (327.3 ns .. 336.6 ns)
std dev              15.66 ns   (14.10 ns .. 17.69 ns)
variance introduced by outliers: 66% (severely inflated)

benchmarking Consing/Data.List 0..1000
time                 9.094 μs   (9.027 μs .. 9.182 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 9.102 μs   (9.018 μs .. 9.215 μs)
std dev              318.3 ns   (229.0 ns .. 435.5 ns)
variance introduced by outliers: 42% (moderately inflated)

benchmarking Consing/Data.Vector 0..1000
time                 509.1 μs   (504.9 μs .. 513.8 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 512.5 μs   (508.5 μs .. 517.7 μs)
std dev              15.54 μs   (12.83 μs .. 20.42 μs)
variance introduced by outliers: 23% (moderately inflated)

benchmarking Consing/Data.Vector.Unboxed 0..1000
time                 224.3 μs   (221.6 μs .. 226.7 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 223.6 μs   (222.1 μs .. 225.2 μs)
std dev              5.376 μs   (4.403 μs .. 6.613 μs)
variance introduced by outliers: 18% (moderately inflated)

benchmarking Consing/Data.List 0..10000
time                 159.2 μs   (157.9 μs .. 160.6 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 160.0 μs   (158.9 μs .. 161.4 μs)
std dev              3.978 μs   (3.467 μs .. 4.973 μs)
variance introduced by outliers: 20% (moderately inflated)

benchmarking Consing/Data.Vector 0..10000
time                 73.53 ms   (72.46 ms .. 74.48 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 73.37 ms   (73.00 ms .. 73.97 ms)
std dev              790.8 μs   (447.2 μs .. 1.239 ms)

benchmarking Consing/Data.Vector.Unboxed 0..10000
time                 24.34 ms   (24.07 ms .. 24.54 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 24.62 ms   (24.48 ms .. 24.87 ms)
std dev              385.9 μs   (231.2 μs .. 575.7 μs)

benchmarking Replicate/Data.List 0..10
time                 8.778 ns   (8.730 ns .. 8.818 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.755 ns   (8.704 ns .. 8.821 ns)
std dev              187.3 ps   (148.4 ps .. 236.5 ps)
variance introduced by outliers: 34% (moderately inflated)

benchmarking Replicate/Data.Vector 0..10
time                 8.671 ns   (8.577 ns .. 8.769 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 8.697 ns   (8.626 ns .. 8.792 ns)
std dev              290.0 ps   (207.1 ps .. 473.2 ps)
variance introduced by outliers: 56% (severely inflated)

benchmarking Replicate/Data.Vector.Unboxed 0..10
time                 8.567 ns   (8.503 ns .. 8.649 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 8.611 ns   (8.556 ns .. 8.709 ns)
std dev              235.5 ps   (176.5 ps .. 324.7 ps)
variance introduced by outliers: 46% (moderately inflated)

benchmarking Replicate/Data.List 0..1000
time                 8.736 ns   (8.667 ns .. 8.813 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 8.790 ns   (8.696 ns .. 8.905 ns)
std dev              344.8 ps   (288.5 ps .. 427.3 ps)
variance introduced by outliers: 64% (severely inflated)

benchmarking Replicate/Data.Vector 0..1000
time                 8.731 ns   (8.664 ns .. 8.789 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 8.751 ns   (8.679 ns .. 8.843 ns)
std dev              271.3 ps   (196.4 ps .. 396.8 ps)
variance introduced by outliers: 52% (severely inflated)

benchmarking Replicate/Data.Vector.Unboxed 0..1000
time                 8.993 ns   (8.874 ns .. 9.096 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 8.914 ns   (8.806 ns .. 9.062 ns)
std dev              425.2 ps   (359.5 ps .. 532.0 ps)
variance introduced by outliers: 72% (severely inflated)

benchmarking Replicate/Data.List 0..10000
time                 9.484 ns   (9.328 ns .. 9.623 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 9.350 ns   (9.192 ns .. 9.510 ns)
std dev              532.0 ps   (456.4 ps .. 623.2 ps)
variance introduced by outliers: 79% (severely inflated)

benchmarking Replicate/Data.Vector 0..10000
time                 8.711 ns   (8.659 ns .. 8.766 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 8.808 ns   (8.730 ns .. 8.921 ns)
std dev              325.1 ps   (232.7 ps .. 478.5 ps)
variance introduced by outliers: 61% (severely inflated)

benchmarking Replicate/Data.Vector.Unboxed 0..10000
time                 8.870 ns   (8.689 ns .. 9.056 ns)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 8.734 ns   (8.641 ns .. 8.849 ns)
std dev              341.9 ps   (254.7 ps .. 460.9 ps)
variance introduced by outliers: 64% (severely inflated)
```
