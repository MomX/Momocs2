# Get rectangularity

Calculate rectangularity as the ratio of area to bounding box area.

## Usage

``` r
get_rectangularity(x, ..., .cols = NULL)
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

Rectangularity = Area / (Length × Width). Value of 1 for a perfect
rectangle. Lower values indicate less rectangular shapes.

## See also

[`get_length()`](https://momx.github.io/Momocs2/reference/get_length.md),
[`get_width()`](https://momx.github.io/Momocs2/reference/get_width.md),
[`get_elongation()`](https://momx.github.io/Momocs2/reference/get_elongation.md)

## Examples

``` r
get_rectangularity(shapes$cat)
#>    length 
#> 0.5527956 

# Use in measure()
bot %>% measure("rectangularity")
#> # A tibble: 40 × 4
#>    coo     type   dummy coo_rectangularity
#>    <out>   <fct>  <fct>              <dbl>
#>  1 (138·2) whisky a                  0.775
#>  2 (168·2) whisky a                  0.777
#>  3 (189·2) whisky a                  0.770
#>  4 (129·2) whisky a                  0.793
#>  5 (152·2) whisky a                  0.651
#>  6 (161·2) whisky a                  0.784
#>  7 (124·2) whisky a                  0.805
#>  8 (126·2) whisky a                  0.766
#>  9 (183·2) whisky a                  0.762
#> 10 (193·2) whisky a                  0.785
#> # ℹ 30 more rows
```
