# Get rugosity

Calculate rugosity as the ratio of perimeter to convex hull perimeter.

## Usage

``` r
get_rugosity(x, ..., .cols = NULL)
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

Rugosity = Perimeter / Perimeter(convex_hull). Value of 1 for convex
shapes. Higher values indicate more irregular/jagged outlines. Note:
This is the inverse of convexity.

## See also

[`get_convexity()`](https://momx.github.io/Momocs2/reference/get_convexity.md),
[`get_chull()`](https://momx.github.io/Momocs2/reference/get_chull.md)

## Examples

``` r
get_rugosity(shapes$cat)
#> [1] 1.368755

# Use in measure()
bot %>% measure("rugosity")
#> # A tibble: 40 × 6
#>    id           coo       type   fake  price coo_rugosity
#>    <chr>        <out>     <fct>  <fct> <dbl>        <dbl>
#>  1 brahma       (138 x 2) whisky a       3           1.01
#>  2 caney        (168 x 2) whisky a       1.2         1.01
#>  3 chimay       (189 x 2) whisky a       3.8         1.02
#>  4 corona       (129 x 2) whisky a       2.6         1.01
#>  5 deusventrue  (152 x 2) whisky a       1.1         1.01
#>  6 duvel        (161 x 2) whisky a       3.1         1.01
#>  7 franziskaner (124 x 2) whisky a       2.6         1.00
#>  8 grimbergen   (126 x 2) whisky a       2.9         1.02
#>  9 guiness      (183 x 2) whisky a       1.2         1.01
#> 10 hoegardeen   (193 x 2) whisky a       3.6         1.01
#> # ℹ 30 more rows
```
