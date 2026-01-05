# Rotate around a center point

Rotate a shape by a specified angle around a given center point.

## Usage

``` r
coo_rotate_around(x, ..., .cols = NULL, .ldk_col = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

- theta:

  Numeric. Rotation angle in radians. Default is 0.

- center:

  Numeric vector of length 2 (x, y). Center point for rotation. Default
  is the centroid.

## Value

- If `x` is a single matrix: returns the rotated matrix

- If `x` is a list: returns a list of rotated matrices

- If `x` is a tibble: returns the tibble with specified coo columns
  rotated
