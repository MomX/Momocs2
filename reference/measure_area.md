# Measure area of a closed outline

Calculate the area enclosed by a shape using the shoelace formula.

## Usage

``` r
measure_area(x, ..., .cols = NULL, .name = NULL)
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
  uses "colname_area" (e.g., "coo_area").

## Value

- If `x` is a single matrix: returns a numeric scalar

- If `x` is a list: returns a numeric vector of class "meas"

- If `x` is a tibble: returns the tibble with new area column(s) added

## Details

Uses the shoelace formula (also called surveyor's formula) to compute
area. The outline is automatically treated as closed (first point
connects to last).

## See also

[`measure_perim()`](https://momx.github.io/Momocs2/reference/measure_perim.md)
for perimeter;
[`measure_centroid_size()`](https://momx.github.io/Momocs2/reference/measure_centroid_size.md)
for centroid size

## Examples

``` r
measure_area(shapes$cat)
#> [1] 14125.5
measure_area(shapes)
#> # Measurements (n = 4 )
#>     cat     dog   heart   leaf2 
#> 14125.5 14907.5 24277.0 16905.5 
measure_area(bot)
#> # A tibble: 40 × 4
#>    coo             type   dummy coo_area
#>    <out>           <fct>  <fct>    <dbl>
#>  1 <out [138 × 2]> whisky a      234515 
#>  2 <out [168 × 2]> whisky a      201056.
#>  3 <out [189 × 2]> whisky a      119460.
#>  4 <out [129 × 2]> whisky a      119568.
#>  5 <out [152 × 2]> whisky a      165736.
#>  6 <out [161 × 2]> whisky a      114015 
#>  7 <out [124 × 2]> whisky a      149503 
#>  8 <out [126 × 2]> whisky a      147642.
#>  9 <out [183 × 2]> whisky a      130178.
#> 10 <out [193 × 2]> whisky a      219548 
#> # ℹ 30 more rows
measure_area(bot, .name = "area")
#> # A tibble: 40 × 4
#>    coo             type   dummy    area
#>    <out>           <fct>  <fct>   <dbl>
#>  1 <out [138 × 2]> whisky a     234515 
#>  2 <out [168 × 2]> whisky a     201056.
#>  3 <out [189 × 2]> whisky a     119460.
#>  4 <out [129 × 2]> whisky a     119568.
#>  5 <out [152 × 2]> whisky a     165736.
#>  6 <out [161 × 2]> whisky a     114015 
#>  7 <out [124 × 2]> whisky a     149503 
#>  8 <out [126 × 2]> whisky a     147642.
#>  9 <out [183 × 2]> whisky a     130178.
#> 10 <out [193 × 2]> whisky a     219548 
#> # ℹ 30 more rows
measure_area(bot, .cols = "coo", .name = "my_area")
#> # A tibble: 40 × 4
#>    coo             type   dummy my_area
#>    <out>           <fct>  <fct>   <dbl>
#>  1 <out [138 × 2]> whisky a     234515 
#>  2 <out [168 × 2]> whisky a     201056.
#>  3 <out [189 × 2]> whisky a     119460.
#>  4 <out [129 × 2]> whisky a     119568.
#>  5 <out [152 × 2]> whisky a     165736.
#>  6 <out [161 × 2]> whisky a     114015 
#>  7 <out [124 × 2]> whisky a     149503 
#>  8 <out [126 × 2]> whisky a     147642.
#>  9 <out [183 × 2]> whisky a     130178.
#> 10 <out [193 × 2]> whisky a     219548 
#> # ℹ 30 more rows
```
