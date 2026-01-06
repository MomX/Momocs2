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
#> # A tibble: 40 × 4
#>    coo       type   dummy coo_rugosity
#>    <out>     <fct>  <fct>        <dbl>
#>  1 (138 x 2) whisky a             1.01
#>  2 (168 x 2) whisky a             1.01
#>  3 (189 x 2) whisky a             1.02
#>  4 (129 x 2) whisky a             1.01
#>  5 (152 x 2) whisky a             1.01
#>  6 (161 x 2) whisky a             1.01
#>  7 (124 x 2) whisky a             1.00
#>  8 (126 x 2) whisky a             1.02
#>  9 (183 x 2) whisky a             1.01
#> 10 (193 x 2) whisky a             1.01
#> # ℹ 30 more rows
```
