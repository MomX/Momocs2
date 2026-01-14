# Get convexity

Calculate convexity as the ratio of convex hull perimeter to shape
perimeter.

## Usage

``` r
get_convexity(x, ..., .cols = NULL)
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

- If `x` is a single matrix: returns a numeric scalar

- If `x` is a list: returns a numeric vector

- If `x` is a tibble: returns a numeric vector extracted from coo column

## Details

Convexity = Perimeter(convex_hull) / Perimeter(shape). Value of 1 for
convex shapes. Lower values indicate concavities.

## See also

[`get_solidity()`](https://momx.github.io/Momocs2/reference/get_solidity.md),
[`get_chull()`](https://momx.github.io/Momocs2/reference/get_chull.md)

## Examples

``` r
get_convexity(shapes$cat)
#> [1] 0.7305908

# Use in measure()
bot %>% measure("convexity")
#> # A tibble: 40 × 6
#>    id           coo       type   fake  price coo_convexity
#>    <chr>        <out>     <fct>  <fct> <dbl>         <dbl>
#>  1 brahma       (138 x 2) whisky a       3           0.991
#>  2 caney        (168 x 2) whisky a       1.2         0.990
#>  3 chimay       (189 x 2) whisky a       3.8         0.985
#>  4 corona       (129 x 2) whisky a       2.6         0.993
#>  5 deusventrue  (152 x 2) whisky a       1.1         0.989
#>  6 duvel        (161 x 2) whisky a       3.1         0.987
#>  7 franziskaner (124 x 2) whisky a       2.6         0.995
#>  8 grimbergen   (126 x 2) whisky a       2.9         0.985
#>  9 guiness      (183 x 2) whisky a       1.2         0.986
#> 10 hoegardeen   (193 x 2) whisky a       3.6         0.993
#> # ℹ 30 more rows
```
