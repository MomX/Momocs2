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
#>    width 
#> 111.1204 

# Use in measure()
bot %>% measure("width")
#> # A tibble: 40 × 4
#>    coo     type   dummy coo_width
#>    <out>   <fct>  <fct>     <dbl>
#>  1 (138·2) whisky a          278.
#>  2 (168·2) whisky a          260.
#>  3 (189·2) whisky a          241.
#>  4 (129·2) whisky a          187.
#>  5 (152·2) whisky a          287.
#>  6 (161·2) whisky a          240.
#>  7 (124·2) whisky a          215.
#>  8 (126·2) whisky a          252.
#>  9 (183·2) whisky a          230.
#> 10 (193·2) whisky a          267.
#> # ℹ 30 more rows
```
