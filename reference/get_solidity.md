# Get solidity

Calculate solidity as the ratio of shape area to convex hull area.

## Usage

``` r
get_solidity(x, ..., .cols = NULL)
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

Solidity = Area(shape) / Area(convex_hull). Value of 1 for convex
shapes. Lower values indicate concavities or holes.

## See also

[`get_convexity()`](https://momx.github.io/Momocs2/reference/get_convexity.md),
[`get_chull()`](https://momx.github.io/Momocs2/reference/get_chull.md)

## Examples

``` r
get_solidity(shapes$cat)
#> [1] 0.787616

# Use in measure()
bot %>% measure("solidity")
#> # A tibble: 40 × 4
#>    coo       type   dummy coo_solidity
#>    <out>     <fct>  <fct>        <dbl>
#>  1 (138 x 2) whisky a            0.893
#>  2 (168 x 2) whisky a            0.920
#>  3 (189 x 2) whisky a            0.928
#>  4 (129 x 2) whisky a            0.944
#>  5 (152 x 2) whisky a            0.875
#>  6 (161 x 2) whisky a            0.954
#>  7 (124 x 2) whisky a            0.949
#>  8 (126 x 2) whisky a            0.922
#>  9 (183 x 2) whisky a            0.916
#> 10 (193 x 2) whisky a            0.933
#> # ℹ 30 more rows
```
