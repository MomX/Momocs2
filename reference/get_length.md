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
#>   length 
#> 229.9564 

# Use in measure()
bot %>% measure("length")
#> # A tibble: 40 × 4
#>    coo     type   dummy coo_length
#>    <out>   <fct>  <fct>      <dbl>
#>  1 (138·2) whisky a          1088.
#>  2 (168·2) whisky a           994.
#>  3 (189·2) whisky a           644.
#>  4 (129·2) whisky a           806.
#>  5 (152·2) whisky a           886.
#>  6 (161·2) whisky a           606.
#>  7 (124·2) whisky a           865.
#>  8 (126·2) whisky a           765.
#>  9 (183·2) whisky a           742.
#> 10 (193·2) whisky a          1048.
#> # ℹ 30 more rows
```
