# Measure normalized centroid size

Calculate the normalized centroid size (centroid size divided by
perimeter).

## Usage

``` r
measure_centroid_size_norm(x, ..., .cols = NULL, .name = NULL)
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
  uses "colname_centroid_size_norm" (e.g., "coo_centroid_size_norm").

## Value

- If `x` is a single matrix: returns a numeric scalar

- If `x` is a list: returns a numeric vector of class "meas"

- If `x` is a tibble: returns the tibble with new normalized centroid
  size column(s) added

## Details

Normalized centroid size provides a scale-independent measure that
accounts for both size (centroid size) and shape complexity (perimeter).

## See also

[`measure_centroid_size()`](https://momx.github.io/Momocs2/reference/measure_centroid_size.md)
for centroid size;
[`get_centroid_size_norm()`](https://momx.github.io/Momocs2/reference/get_centroid_size_norm.md)
for the getter function

## Examples

``` r
measure_centroid_size_norm(shapes$cat)
#> [1] 77.4076
measure_centroid_size_norm(shapes)
#> # Measurements (n = 4 )
#>      cat      dog    heart    leaf2 
#> 77.40760 86.78916 89.28050 74.38899 
measure_centroid_size_norm(bot)
#> # A tibble: 40 × 4
#>    coo             type   dummy coo_centroid_size_norm
#>    <out>           <fct>  <fct>                  <dbl>
#>  1 <out [138 × 2]> whisky a                       364.
#>  2 <out [168 × 2]> whisky a                       333.
#>  3 <out [189 × 2]> whisky a                       232.
#>  4 <out [129 × 2]> whisky a                       267.
#>  5 <out [152 × 2]> whisky a                       300.
#>  6 <out [161 × 2]> whisky a                       220.
#>  7 <out [124 × 2]> whisky a                       290.
#>  8 <out [126 × 2]> whisky a                       268.
#>  9 <out [183 × 2]> whisky a                       257.
#> 10 <out [193 × 2]> whisky a                       353.
#> # ℹ 30 more rows
measure_centroid_size_norm(bot, .name = "CS_norm")
#> # A tibble: 40 × 4
#>    coo             type   dummy CS_norm
#>    <out>           <fct>  <fct>   <dbl>
#>  1 <out [138 × 2]> whisky a        364.
#>  2 <out [168 × 2]> whisky a        333.
#>  3 <out [189 × 2]> whisky a        232.
#>  4 <out [129 × 2]> whisky a        267.
#>  5 <out [152 × 2]> whisky a        300.
#>  6 <out [161 × 2]> whisky a        220.
#>  7 <out [124 × 2]> whisky a        290.
#>  8 <out [126 × 2]> whisky a        268.
#>  9 <out [183 × 2]> whisky a        257.
#> 10 <out [193 × 2]> whisky a        353.
#> # ℹ 30 more rows
```
