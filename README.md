# bench

## Containers

For container benchmarks:

    $ stack bench :containers

For specific container benchmarks:

    $ stack bench :containers --benchmark-arguments Consing

### Results

```
benchmarking Consing/Data.List 0..10
time                 95.06 ns   (94.09 ns .. 96.05 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 95.73 ns   (94.73 ns .. 97.13 ns)
std dev              3.957 ns   (3.010 ns .. 5.514 ns)
variance introduced by outliers: 62% (severely inflated)

benchmarking Consing/Data.Vector 0..10
time                 421.3 ns   (412.3 ns .. 431.3 ns)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 416.6 ns   (410.6 ns .. 423.5 ns)
std dev              22.41 ns   (19.19 ns .. 26.43 ns)
variance introduced by outliers: 71% (severely inflated)

benchmarking Consing/Data.Vector.Unboxed 0..10
time                 324.5 ns   (320.7 ns .. 328.0 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 323.9 ns   (320.7 ns .. 328.7 ns)
std dev              12.99 ns   (8.653 ns .. 22.10 ns)
variance introduced by outliers: 58% (severely inflated)

benchmarking Consing/Data.List 0..1000
time                 9.790 μs   (9.668 μs .. 9.884 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 9.608 μs   (9.466 μs .. 9.756 μs)
std dev              469.6 ns   (405.2 ns .. 577.2 ns)
variance introduced by outliers: 60% (severely inflated)

benchmarking Consing/Data.Vector 0..1000
time                 556.6 μs   (544.8 μs .. 566.7 μs)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 536.4 μs   (527.9 μs .. 546.4 μs)
std dev              31.50 μs   (27.69 μs .. 35.84 μs)
variance introduced by outliers: 51% (severely inflated)

benchmarking Consing/Data.Vector.Unboxed 0..1000
time                 229.6 μs   (225.9 μs .. 233.9 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 236.5 μs   (233.6 μs .. 239.7 μs)
std dev              10.19 μs   (8.704 μs .. 12.69 μs)
variance introduced by outliers: 41% (moderately inflated)

benchmarking Consing/Data.List 0..10000
time                 166.2 μs   (163.0 μs .. 168.8 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 162.7 μs   (161.1 μs .. 164.5 μs)
std dev              5.827 μs   (4.992 μs .. 6.859 μs)
variance introduced by outliers: 33% (moderately inflated)

benchmarking Consing/Data.Vector 0..10000
time                 75.95 ms   (72.65 ms .. 79.78 ms)
                     0.996 R²   (0.992 R² .. 1.000 R²)
mean                 75.00 ms   (74.15 ms .. 76.44 ms)
std dev              1.895 ms   (1.171 ms .. 2.861 ms)

benchmarking Consing/Data.Vector.Unboxed 0..10000
time                 25.32 ms   (24.39 ms .. 26.60 ms)
                     0.995 R²   (0.990 R² .. 0.999 R²)
mean                 25.29 ms   (25.01 ms .. 25.67 ms)
std dev              743.2 μs   (561.8 μs .. 1.124 ms)
```
