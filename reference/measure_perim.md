# Measure perimeter of an outline

Calculate the total perimeter (length) of a shape.

## Usage

``` r
measure_perim(x, ..., .cols = NULL, .name = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

- .name:

  Character. Name for the new column when `x` is a tibble. If `NULL`,
  uses "colname_perim" (e.g., "coo_perim").

## Value

- If `x` is a single matrix: returns a numeric scalar

- If `x` is a list: returns a numeric vector of class "meas"

- If `x` is a tibble: returns the tibble with new perimeter column(s)
  added

## Details

Computes the sum of Euclidean distances between consecutive points,
including the distance from the last point back to the first.

## See also

[`measure_area()`](https://momx.github.io/Momocs2/reference/measure_area.md)
for area;
[`get_perim()`](https://momx.github.io/Momocs2/reference/get_perim.md)
for the getter function

## Examples

``` r
measure_perim(shapes$cat)
#> [1] 750.7288
measure_perim(shapes)
#> # Measurements (n = 4 )
#>      cat      dog    heart    leaf2 
#> 750.7288 827.6342 640.2559 831.2915 
measure_perim(bot)
#> # A tibble: 40 × 4
#>    coo             type   dummy coo_perim
#>    <out>           <fct>  <fct>     <dbl>
#>  1 <out [138 × 2]> whisky a         2514.
#>  2 <out [168 × 2]> whisky a         2289.
#>  3 <out [189 × 2]> whisky a         1593.
#>  4 <out [129 × 2]> whisky a         1838.
#>  5 <out [152 × 2]> whisky a         2087.
#>  6 <out [161 × 2]> whisky a         1509.
#>  7 <out [124 × 2]> whisky a         1985.
#>  8 <out [126 × 2]> whisky a         1848.
#>  9 <out [183 × 2]> whisky a         1767.
#> 10 <out [193 × 2]> whisky a         2424.
#> # ℹ 30 more rows
measure_perim(bot, .name = "perimeter")
#> # A tibble: 40 × 4
#>    coo             type   dummy perimeter
#>    <out>           <fct>  <fct>     <dbl>
#>  1 <out [138 × 2]> whisky a         2514.
#>  2 <out [168 × 2]> whisky a         2289.
#>  3 <out [189 × 2]> whisky a         1593.
#>  4 <out [129 × 2]> whisky a         1838.
#>  5 <out [152 × 2]> whisky a         2087.
#>  6 <out [161 × 2]> whisky a         1509.
#>  7 <out [124 × 2]> whisky a         1985.
#>  8 <out [126 × 2]> whisky a         1848.
#>  9 <out [183 × 2]> whisky a         1767.
#> 10 <out [193 × 2]> whisky a         2424.
#> # ℹ 30 more rows
```
