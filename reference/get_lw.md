# Get length and width

Calculate length and width based on inertia axes (PCA alignment).

## Usage

``` r
get_lw(x, ..., .cols = NULL)
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

- If `x` is a single matrix: returns a numeric vector c(length, width)

- If `x` is a list: returns a list of numeric vectors

- If `x` is a tibble: returns a list of numeric vectors extracted from
  coo column

## Details

Length is defined as the range along the major inertia axis (largest
variance). Width is defined as the range along the minor inertia axis
(smallest variance). Shape is first centered and aligned using SVD
before computing ranges.

Note: Returns a vector, not a scalar, so cannot be used with
[`measure()`](https://momx.github.io/Momocs2/reference/measure.md). Use
[`get_length()`](https://momx.github.io/Momocs2/reference/get_length.md),
[`get_width()`](https://momx.github.io/Momocs2/reference/get_width.md),
or
[`get_elongation()`](https://momx.github.io/Momocs2/reference/get_elongation.md)
for scalar measurements.

## See also

[`get_length()`](https://momx.github.io/Momocs2/reference/get_length.md),
[`get_width()`](https://momx.github.io/Momocs2/reference/get_width.md),
[`get_elongation()`](https://momx.github.io/Momocs2/reference/get_elongation.md)

## Examples

``` r
get_lw(shapes$cat)
#>   length    width 
#> 229.9564 111.1204 
get_lw(shapes)
#> $cat
#>   length    width 
#> 229.9564 111.1204 
#> 
#> $dog
#>   length    width 
#> 258.8037 162.4473 
#> 
#> $heart
#>   length    width 
#> 207.0122 178.9135 
#> 
#> $leaf2
#>   length    width 
#> 206.1788 172.7478 
#> 
```
