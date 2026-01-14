# Get number of coordinates

Count the number of coordinate points in a shape.

## Usage

``` r
get_coords_nb(x, ..., .cols = NULL)
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

- If `x` is a single matrix: returns an integer

- If `x` is a list: returns a numeric vector

- If `x` is a tibble: returns a numeric vector extracted from coo column

## Details

Simply returns the number of rows (coordinate points) in the shape. Can
be used with
[`measure()`](https://momx.github.io/Momocs2/reference/measure.md).

## Examples

``` r
get_coords_nb(shapes$cat)
#> [1] 120
get_coords_nb(shapes)
#> [1] 120 120 120 120

# Use in measure()
bot %>% measure("coords_nb")
#> # A tibble: 40 × 6
#>    id           coo       type   fake  price coo_coords_nb
#>    <chr>        <out>     <fct>  <fct> <dbl>         <dbl>
#>  1 brahma       (138 x 2) whisky a       3             138
#>  2 caney        (168 x 2) whisky a       1.2           168
#>  3 chimay       (189 x 2) whisky a       3.8           189
#>  4 corona       (129 x 2) whisky a       2.6           129
#>  5 deusventrue  (152 x 2) whisky a       1.1           152
#>  6 duvel        (161 x 2) whisky a       3.1           161
#>  7 franziskaner (124 x 2) whisky a       2.6           124
#>  8 grimbergen   (126 x 2) whisky a       2.9           126
#>  9 guiness      (183 x 2) whisky a       1.2           183
#> 10 hoegardeen   (193 x 2) whisky a       3.6           193
#> # ℹ 30 more rows
```
