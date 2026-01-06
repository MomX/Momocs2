# Sample at regular angles from centroid

Sample points at regular angular intervals from the centroid (radial
sampling).

## Usage

``` r
coo_sample_regular_radius(x, ..., .cols = NULL, .ldk_col = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo columns.

- .ldk_col:

  Character. Name of landmark column. If `NULL`, uses `colname_ldk`.

- n:

  Integer. Number of points to sample.

## Value

- If `x` is a single matrix: returns the sampled matrix

- If `x` is a list: returns a list of sampled matrices

- If `x` is a tibble: returns the tibble with sampled coo column(s)

## Details

Samples points at regular angular intervals from the centroid, like
spokes on a wheel. For each target angle, finds the outline point
closest to that direction.

Landmarks are NOT preserved - this resampling method fundamentally
changes the point structure.

## See also

[`coo_sample()`](https://momx.github.io/Momocs2/reference/coo_sample.md)

## Examples

``` r
coo_sample_regular_radius(shapes$cat, n = 64)
#>       [,1] [,2]
#>  [1,]  221   97
#>  [2,]  221   97
#>  [3,]  221  102
#>  [4,]  221  102
#>  [5,]  222  108
#>  [6,]  224  114
#>  [7,]  227  120
#>  [8,]  235  132
#>  [9,]  237  144
#> [10,]  237  156
#> [11,]  235  167
#> [12,]  244  199
#> [13,]  234  208
#> [14,]  226  220
#> [15,]  212  220
#> [16,]  197  205
#> [17,]  188  176
#> [18,]  183  170
#> [19,]  177  165
#> [20,]  165  160
#> [21,]  159  157
#> [22,]  159  157
#> [23,]  153  151
#> [24,]  148  145
#> [25,]  144  139
#> [26,]  142  134
#> [27,]  139  128
#> [28,]  136  122
#> [29,]  134  116
#> [30,]  134  110
#> [31,]  133  104
#> [32,]  133   98
#> [33,]  133   92
#> [34,]  133   92
#> [35,]  133   92
#> [36,]  133   92
#> [37,]  133   92
#> [38,]  133   92
#> [39,]  133   92
#> [40,]  133   92
#> [41,]  133   92
#> [42,]  133   92
#> [43,]  133   92
#> [44,]  133   92
#> [45,]  133   92
#> [46,]  133   92
#> [47,]  133   92
#> [48,]  133   92
#> [49,]  133   92
#> [50,]  133   92
#> [51,]  133   92
#> [52,]  133   92
#> [53,]  133   92
#> [54,]  133   92
#> [55,]  133   92
#> [56,]  133   92
#> [57,]  133   92
#> [58,]  133   92
#> [59,]  133   92
#> [60,]  133   92
#> [61,]  133   92
#> [62,]  133   92
#> [63,]  133   92
#> [64,]  133   92
#> attr(,"class")
#> [1] "coo"    "matrix" "array" 
```
