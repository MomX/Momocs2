# Get width

Calculate width along the minor inertia axis.

## Usage

``` r
get_width(x, ..., .cols = NULL)
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

Width is the range along the minor inertia axis (smallest variance
direction).

## See also

[`get_length()`](https://momx.github.io/Momocs2/reference/get_length.md),
[`get_lw()`](https://momx.github.io/Momocs2/reference/get_lw.md),
[`get_elongation()`](https://momx.github.io/Momocs2/reference/get_elongation.md)

## Examples

``` r
get_width(shapes$cat)
#> [1] 111.1204

# Use in measure()
bot %>% measure("width")
#> # A tibble: 40 × 6
#>    id           coo       type   fake  price coo_width
#>    <chr>        <out>     <fct>  <fct> <dbl>     <dbl>
#>  1 brahma       (138 x 2) whisky a       3        278.
#>  2 caney        (168 x 2) whisky a       1.2      260.
#>  3 chimay       (189 x 2) whisky a       3.8      241.
#>  4 corona       (129 x 2) whisky a       2.6      187.
#>  5 deusventrue  (152 x 2) whisky a       1.1      287.
#>  6 duvel        (161 x 2) whisky a       3.1      240.
#>  7 franziskaner (124 x 2) whisky a       2.6      215.
#>  8 grimbergen   (126 x 2) whisky a       2.9      252.
#>  9 guiness      (183 x 2) whisky a       1.2      230.
#> 10 hoegardeen   (193 x 2) whisky a       3.6      267.
#> # ℹ 30 more rows
```
