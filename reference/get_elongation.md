# Get elongation

Calculate elongation (aspect ratio) as length / width.

## Usage

``` r
get_elongation(x, ..., .cols = NULL)
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

Elongation is the aspect ratio based on inertia axes: length / width.
Value of 1 indicates equal length and width. Higher values indicate more
elongated shapes.

This is equivalent to eccentricity based on bounding box dimensions.

## See also

[`get_length()`](https://momx.github.io/Momocs2/reference/get_length.md),
[`get_width()`](https://momx.github.io/Momocs2/reference/get_width.md),
[`get_rectangularity()`](https://momx.github.io/Momocs2/reference/get_rectangularity.md)

## Examples

``` r
get_elongation(shapes$cat)
#> [1] 2.069434

# Use in measure()
bot %>% measure("elongation")
#> # A tibble: 40 × 6
#>    id           coo       type   fake  price coo_elongation
#>    <chr>        <out>     <fct>  <fct> <dbl>          <dbl>
#>  1 brahma       (138 x 2) whisky a       3             3.91
#>  2 caney        (168 x 2) whisky a       1.2           3.82
#>  3 chimay       (189 x 2) whisky a       3.8           2.67
#>  4 corona       (129 x 2) whisky a       2.6           4.31
#>  5 deusventrue  (152 x 2) whisky a       1.1           3.09
#>  6 duvel        (161 x 2) whisky a       3.1           2.52
#>  7 franziskaner (124 x 2) whisky a       2.6           4.03
#>  8 grimbergen   (126 x 2) whisky a       2.9           3.04
#>  9 guiness      (183 x 2) whisky a       1.2           3.23
#> 10 hoegardeen   (193 x 2) whisky a       3.6           3.93
#> # ℹ 30 more rows
```
