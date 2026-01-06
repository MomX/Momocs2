# Get compactness

Calculate shape compactness (Miller's index).

## Usage

``` r
get_compactness(x, ..., .cols = NULL)
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

Compactness (Miller's index) = Area / Area(bounding_box). Value of 1 for
shapes that completely fill their bounding box (rectangles). Lower
values indicate less compact shapes.

Note: Polsby-Popper compactness (4π × Area / Perimeter²) is available as
[`get_circularity()`](https://momx.github.io/Momocs2/reference/get_circularity.md).

## See also

[`get_circularity()`](https://momx.github.io/Momocs2/reference/get_circularity.md),
[`get_rectangularity()`](https://momx.github.io/Momocs2/reference/get_rectangularity.md)

## Examples

``` r
get_compactness(shapes$cat)
#> [1] 0.5346923

# Use in measure()
bot %>% measure("compactness")
#> # A tibble: 40 × 4
#>    coo     type   dummy coo_compactness
#>    <out>   <fct>  <fct>           <dbl>
#>  1 (138·2) whisky a               0.762
#>  2 (168·2) whisky a               0.775
#>  3 (189·2) whisky a               0.765
#>  4 (129·2) whisky a               0.793
#>  5 (152·2) whisky a               0.652
#>  6 (161·2) whisky a               0.784
#>  7 (124·2) whisky a               0.808
#>  8 (126·2) whisky a               0.766
#>  9 (183·2) whisky a               0.763
#> 10 (193·2) whisky a               0.785
#> # ℹ 30 more rows
```
