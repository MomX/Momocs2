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
#>   length 
#> 2.069434 

# Use in measure()
bot %>% measure("elongation")
#> # A tibble: 40 × 4
#>    coo     type   dummy coo_elongation
#>    <out>   <fct>  <fct>          <dbl>
#>  1 (138·2) whisky a               3.91
#>  2 (168·2) whisky a               3.82
#>  3 (189·2) whisky a               2.67
#>  4 (129·2) whisky a               4.31
#>  5 (152·2) whisky a               3.09
#>  6 (161·2) whisky a               2.52
#>  7 (124·2) whisky a               4.03
#>  8 (126·2) whisky a               3.04
#>  9 (183·2) whisky a               3.23
#> 10 (193·2) whisky a               3.93
#> # ℹ 30 more rows
```
