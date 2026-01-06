# Get range span

Calculate the span (max - min) for each coordinate axis.

## Usage

``` r
get_range_diff(x, ..., .cols = NULL)
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

- If `x` is a single matrix: returns a numeric vector of length 2
  (x_span, y_span)

- If `x` is a list: returns a list of numeric vectors

- If `x` is a tibble: returns a list of numeric vectors extracted from
  coo column

## Details

Returns the absolute distance spanned along each axis. This is not a
scalar (it's a 2-element vector), so cannot be used with
[`measure()`](https://momx.github.io/Momocs2/reference/measure.md).

## See also

[`get_range()`](https://momx.github.io/Momocs2/reference/get_range.md)

## Examples

``` r
get_range_diff(shapes$cat)
#> [1] 119 222
get_range_diff(shapes)
#> $cat
#> [1] 119 222
#> 
#> $dog
#> [1] 212 182
#> 
#> $heart
#> [1] 207 179
#> 
#> $leaf2
#> [1] 173 206
#> 
```
