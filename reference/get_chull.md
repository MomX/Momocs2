# Get convex hull

Calculate the convex hull coordinates or indices.

## Usage

``` r
get_chull(x, ..., .cols = NULL)

get_chull_id(x, ..., .cols = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

## Value

- `get_chull()`: convex hull coordinates as a matrix

- `get_chull_id()`: indices of convex hull points as an integer vector

## Details

Uses [`grDevices::chull()`](https://rdrr.io/r/grDevices/chull.html) to
compute the convex hull. The convex hull is the smallest convex polygon
containing all points.

Note: Returns non-scalar values, so cannot be used with
[`measure()`](https://momx.github.io/Momocs2/reference/measure.md).

## See also

[`get_convexity()`](https://momx.github.io/Momocs2/reference/get_convexity.md),
[`get_solidity()`](https://momx.github.io/Momocs2/reference/get_solidity.md)

## Examples

``` r
get_chull(shapes$cat)
#>       [,1] [,2]
#>  [1,]  232   55
#>  [2,]  230   49
#>  [3,]  208   19
#>  [4,]  202   15
#>  [5,]  190   11
#>  [6,]  178    9
#>  [7,]  166    9
#>  [8,]  154   11
#>  [9,]  149   13
#> [10,]  143   16
#> [11,]  137   21
#> [12,]  133   27
#> [13,]  130   33
#> [14,]  130   45
#> [15,]  133  104
#> [16,]  134  116
#> [17,]  136  122
#> [18,]  144  139
#> [19,]  148  145
#> [20,]  197  205
#> [21,]  216  226
#> [22,]  221  231
#> [23,]  225  226
#> [24,]  249  195
get_chull_id(shapes$cat)
#>  [1] 113 114  30  31  33  35  37  39  40  41  42  43  44  46  56  58  59  62  63
#> [20]  75  79  80  81  88
```
