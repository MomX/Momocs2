# Get coordinate ranges

Calculate the range (min, max) for each coordinate axis.

## Usage

``` r
get_range(x, ..., .cols = NULL)
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

- If `x` is a single matrix: returns a 2x2 matrix (rows: min/max, cols:
  x/y)

- If `x` is a list: returns a list of 2x2 matrices

- If `x` is a tibble: returns a list of 2x2 matrices extracted from coo
  column

## Details

Returns a matrix where row 1 is min values and row 2 is max values for x
and y. This is not a scalar, so cannot be used with
[`measure()`](https://momx.github.io/Momocs2/reference/measure.md).

## See also

[`get_range_diff()`](https://momx.github.io/Momocs2/reference/get_range_diff.md)

## Examples

``` r
get_range(shapes$cat)
#>      [,1] [,2]
#> [1,]  130    9
#> [2,]  249  231
get_range(shapes)
#> $cat
#>      [,1] [,2]
#> [1,]  130    9
#> [2,]  249  231
#> 
#> $dog
#>      [,1] [,2]
#> [1,]  104   45
#> [2,]  316  227
#> 
#> $heart
#>      [,1] [,2]
#> [1,]   99   34
#> [2,]  306  213
#> 
#> $leaf2
#>      [,1] [,2]
#> [1,]  109   20
#> [2,]  282  226
#> 
```
