# Get length

Calculate length along the major inertia axis.

## Usage

``` r
get_length(x, ..., .cols = NULL)
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

Length is the range along the major inertia axis (largest variance
direction).

## See also

[`get_width()`](https://momx.github.io/Momocs2/reference/get_width.md),
[`get_lw()`](https://momx.github.io/Momocs2/reference/get_lw.md),
[`get_elongation()`](https://momx.github.io/Momocs2/reference/get_elongation.md)

## Examples

``` r
get_length(shapes$cat)
#> [1] 229.9564

# Use in measure()
bot %>% measure("length")
#> # A tibble: 40 × 6
#>    id           coo       type   fake  price coo_length
#>    <chr>        <out>     <fct>  <fct> <dbl>      <dbl>
#>  1 brahma       (138 x 2) whisky a       3        1088.
#>  2 caney        (168 x 2) whisky a       1.2       994.
#>  3 chimay       (189 x 2) whisky a       3.8       644.
#>  4 corona       (129 x 2) whisky a       2.6       806.
#>  5 deusventrue  (152 x 2) whisky a       1.1       886.
#>  6 duvel        (161 x 2) whisky a       3.1       606.
#>  7 franziskaner (124 x 2) whisky a       2.6       865.
#>  8 grimbergen   (126 x 2) whisky a       2.9       765.
#>  9 guiness      (183 x 2) whisky a       1.2       742.
#> 10 hoegardeen   (193 x 2) whisky a       3.6      1048.
#> # ℹ 30 more rows
```
