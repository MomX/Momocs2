# Get point closest to a given direction

Find the index of the point closest to a specified direction from the
centroid.

## Usage

``` r
get_closest_direction(x, ..., .cols = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

- direction:

  Character. One of "right", "up", "left", or "down".

## Value

- If `x` is a single matrix: returns an integer (point index)

- If `x` is a list: returns an integer vector

- If `x` is a tibble: returns an integer vector extracted from coo
  column

## Details

Centers the shape, calculates the angle from centroid to each point, and
returns the index of the point whose angle is closest to the specified
direction:

- "right" = 0 radians (East)

- "up" = pi/2 radians (North)

- "left" = pi radians (West)

- "down" = -pi/2 radians (South)

## See also

[`get_closest()`](https://momx.github.io/Momocs2/reference/get_closest.md),
[`get_closest_angle()`](https://momx.github.io/Momocs2/reference/get_closest.md)

## Examples

``` r
# Find point closest to East
get_closest_direction(shapes$cat, direction = "right")
#> [1] 105

# Find point closest to North
get_closest_direction(shapes$cat, direction = "up")
#> [1] 70
```
