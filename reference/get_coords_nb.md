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
#> # A tibble: 40 × 4
#>    coo       type   dummy coo_coords_nb
#>    <out>     <fct>  <fct>         <dbl>
#>  1 (138 x 2) whisky a               138
#>  2 (168 x 2) whisky a               168
#>  3 (189 x 2) whisky a               189
#>  4 (129 x 2) whisky a               129
#>  5 (152 x 2) whisky a               152
#>  6 (161 x 2) whisky a               161
#>  7 (124 x 2) whisky a               124
#>  8 (126 x 2) whisky a               126
#>  9 (183 x 2) whisky a               183
#> 10 (193 x 2) whisky a               193
#> # ℹ 30 more rows
```
