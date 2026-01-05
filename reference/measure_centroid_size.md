# Measure centroid size

Calculate the centroid size: the square root of the sum of squared
distances from each point to the centroid.

## Usage

``` r
measure_centroid_size(x, ..., .cols = NULL, .name = NULL)
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
  uses "colname_centroid_size" (e.g., "coo_centroid_size").

## Value

- If `x` is a single matrix: returns a numeric scalar

- If `x` is a list: returns a numeric vector of class "meas"

- If `x` is a tibble: returns the tibble with new centroid size
  column(s) added

## Details

Centroid size is a common size measure in geometric morphometrics. It is
scale-independent and used for allometric correction.

## See also

[`measure_centroid_size_norm()`](https://momx.github.io/Momocs2/reference/measure_centroid_size_norm.md)
for normalized centroid size;
[`get_centroid_size()`](https://momx.github.io/Momocs2/reference/get_centroid_size.md)
for the getter function

## Examples

``` r
measure_centroid_size(shapes$cat)
#> [1] 847.9577
measure_centroid_size(shapes)
#> # Measurements (n = 4 )
#>      cat      dog    heart    leaf2 
#> 847.9577 950.7276 978.0189 814.8906 
measure_centroid_size(bot)
#> # A tibble: 40 × 4
#>    coo             type   dummy coo_centroid_size
#>    <out>           <fct>  <fct>             <dbl>
#>  1 <out [138 × 2]> whisky a                 4277.
#>  2 <out [168 × 2]> whisky a                 4312.
#>  3 <out [189 × 2]> whisky a                 3193.
#>  4 <out [129 × 2]> whisky a                 3035.
#>  5 <out [152 × 2]> whisky a                 3701.
#>  6 <out [161 × 2]> whisky a                 2796.
#>  7 <out [124 × 2]> whisky a                 3225.
#>  8 <out [126 × 2]> whisky a                 3011.
#>  9 <out [183 × 2]> whisky a                 3472.
#> 10 <out [193 × 2]> whisky a                 4907.
#> # ℹ 30 more rows
measure_centroid_size(bot, .name = "CS")
#> # A tibble: 40 × 4
#>    coo             type   dummy    CS
#>    <out>           <fct>  <fct> <dbl>
#>  1 <out [138 × 2]> whisky a     4277.
#>  2 <out [168 × 2]> whisky a     4312.
#>  3 <out [189 × 2]> whisky a     3193.
#>  4 <out [129 × 2]> whisky a     3035.
#>  5 <out [152 × 2]> whisky a     3701.
#>  6 <out [161 × 2]> whisky a     2796.
#>  7 <out [124 × 2]> whisky a     3225.
#>  8 <out [126 × 2]> whisky a     3011.
#>  9 <out [183 × 2]> whisky a     3472.
#> 10 <out [193 × 2]> whisky a     4907.
#> # ℹ 30 more rows
```
