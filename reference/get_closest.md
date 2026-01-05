# Find closest point to target

Find the point on a shape closest to a target location or angle.

## Usage

``` r
get_closest(x, ..., .cols = NULL)

get_closest_angle(x, ..., .cols = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

- target:

  A numeric vector of length 2 (x, y) or a list with `$x` and `$y`
  (e.g., from [`locator()`](https://rdrr.io/r/graphics/locator.html)).

- theta:

  Numeric. Angle in radians for angular search.

## Value

- If `x` is a single matrix: returns an integer (row index of closest
  point)

- If `x` is a list: returns a numeric vector of integers

- If `x` is a tibble: returns a numeric vector of integers extracted
  from coo column

## Details

- `get_closest()`: finds point closest to spatial target using Euclidean
  distance. Compatible with
  [`locator()`](https://rdrr.io/r/graphics/locator.html) for interactive
  selection.

- `get_closest_angle()`: finds point closest to a specified angle
  direction.

For tibbles, these functions extract values without modifying the
tibble.

## Examples

``` r
get_closest(shapes$cat, c(200, 100))
#> [1] 104
get_closest_angle(shapes$cat, theta = 0)
#> [1] 105

# Extract from tibble
closest_ids <- get_closest(bot, c(100, 100))

if (FALSE) { # \dontrun{
 # Add to tibble when dplyr is loaded
bot %>% mutate(closest = get_closest(coo, c(100, 100)))
} # }
```
