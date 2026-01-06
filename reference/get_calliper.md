# Get calliper

Calculate the maximum distance between any two points (calliper/Feret
diameter).

## Usage

``` r
get_calliper(x, ..., .cols = NULL)

get_calliper_ids(x, ..., .cols = NULL)
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

- `get_calliper()`: maximum distance as a numeric scalar

- `get_calliper_ids()`: indices of the two furthest points as a vector
  c(i, j)

## Details

Calculates the maximum distance between any two points on the outline.
Also known as Feret diameter or maximum calliper distance.

Uses brute force O(n²) algorithm. May be slow for outlines with many
points.

Note: `get_calliper_ids()` returns a vector, not a scalar, so cannot be
used with
[`measure()`](https://momx.github.io/Momocs2/reference/measure.md).

## Examples

``` r
get_calliper(shapes$cat)
#> [1] 229.9761
get_calliper_ids(shapes$cat)
#> [1] 39 80

# Use in measure() on minibot for the sake of speed
bot[1:2, ] %>% measure("calliper")
#> # A tibble: 2 × 4
#>   coo       type   dummy coo_calliper
#>   <out>     <fct>  <fct>        <dbl>
#> 1 (138 x 2) whisky a            1088.
#> 2 (168 x 2) whisky a             994.
```
