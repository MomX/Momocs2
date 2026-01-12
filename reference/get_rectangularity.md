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
#> [1] 0.5527956

# Use in measure() on a mini bot for the sake of speed
bot[1:2, ] %>% measure("rectangularity")
#> # A tibble: 2 × 4
#>   coo       type   dummy coo_rectangularity
#>   <out>     <fct>  <fct>              <dbl>
#> 1 (138 x 2) whisky a                  0.775
#> 2 (168 x 2) whisky a                  0.777
```
