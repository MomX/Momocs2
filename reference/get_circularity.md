# Get circularity measures

Calculate circularity (isoperimetric quotient) and related shape
descriptors.

## Usage

``` r
get_circularity(x, ..., .cols = NULL)

get_circularity_norm(x, ..., .cols = NULL)

get_circularity_haralick(x, ..., .cols = NULL)
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

**Circularity measures:**

- `get_circularity()`: Isoperimetric quotient = 4π × Area / Perimeter².
  Value of 1 for a perfect circle, \< 1 for other shapes.

- `get_circularity_norm()`: Normalized version = Area /
  (Perimeter²/(4π)). Alternative formulation, same interpretation.

- `get_circularity_haralick()`: Mean(radii) / SD(radii) from centroid.
  Higher values = more circular. Sensitive to irregularities.

All measures equal 1 for perfect circles and decrease for other shapes.

## See also

[`measure()`](https://momx.github.io/Momocs2/reference/measure.md) for
adding measurement columns

## Examples

``` r
get_circularity(shapes$cat)
#> [1] 0.3149543
get_circularity_norm(shapes$cat)
#> [1] 0.3149543
get_circularity_haralick(shapes$cat)
#> [1] 2.835053

# Use in measure()
bot %>% measure("circularity")
#> # A tibble: 40 × 4
#>    coo       type   dummy coo_circularity
#>    <out>     <fct>  <fct>           <dbl>
#>  1 (138 x 2) whisky a               0.466
#>  2 (168 x 2) whisky a               0.482
#>  3 (189 x 2) whisky a               0.592
#>  4 (129 x 2) whisky a               0.445
#>  5 (152 x 2) whisky a               0.478
#>  6 (161 x 2) whisky a               0.629
#>  7 (124 x 2) whisky a               0.477
#>  8 (126 x 2) whisky a               0.544
#>  9 (183 x 2) whisky a               0.524
#> 10 (193 x 2) whisky a               0.470
#> # ℹ 30 more rows
bot %>% measure(c("circularity", "circularity_haralick"))
#> # A tibble: 40 × 5
#>    coo       type   dummy coo_circularity coo_circularity_haralick
#>    <out>     <fct>  <fct>           <dbl>                    <dbl>
#>  1 (138 x 2) whisky a               0.466                     2.32
#>  2 (168 x 2) whisky a               0.482                     2.37
#>  3 (189 x 2) whisky a               0.592                     2.94
#>  4 (129 x 2) whisky a               0.445                     2.26
#>  5 (152 x 2) whisky a               0.478                     2.40
#>  6 (161 x 2) whisky a               0.629                     3.12
#>  7 (124 x 2) whisky a               0.477                     2.36
#>  8 (126 x 2) whisky a               0.544                     2.64
#>  9 (183 x 2) whisky a               0.524                     2.59
#> 10 (193 x 2) whisky a               0.470                     2.35
#> # ℹ 30 more rows
```
