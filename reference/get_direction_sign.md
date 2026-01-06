# Get outline direction sign

Determine if an outline is traced counter-clockwise
(positive/trigonometric) or clockwise (negative).

## Usage

``` r
get_direction_sign(x, ..., .cols = NULL)
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

- If `x` is a single matrix: returns a logical (TRUE =
  counter-clockwise)

- If `x` is a list: returns a logical vector

- If `x` is a tibble: returns a logical vector extracted from coo column

## Details

Uses the signed area formula (shoelace). Positive area indicates
counter-clockwise (trigonometric) direction, negative indicates
clockwise.

Returns TRUE for counter-clockwise, FALSE for clockwise.

## See also

[`coo_direction_positive()`](https://momx.github.io/Momocs2/reference/coo_direction.md),
[`coo_direction_negative()`](https://momx.github.io/Momocs2/reference/coo_direction.md)

## Examples

``` r
get_direction_sign(shapes$cat)
#> [1] FALSE
get_direction_sign(shapes)
#> $cat
#> [1] FALSE
#> 
#> $dog
#> [1] FALSE
#> 
#> $heart
#> [1] FALSE
#> 
#> $leaf2
#> [1] FALSE
#> 
```
