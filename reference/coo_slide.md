# Slide coordinates along outline

Rotate the order of points along the outline without changing the shape.

## Usage

``` r
coo_slide(x, ..., .cols = NULL, .ldk_col = NULL)

coo_slide_closest(x, ..., .cols = NULL, .ldk_col = NULL)

coo_slide_angle(x, ..., .cols = NULL, .ldk_col = NULL)

coo_slide_direction(x, ..., .cols = NULL, .ldk_col = NULL)

coo_slide_gap(x, ..., .cols = NULL, .ldk_col = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

- .ldk_col:

  Character. Name of landmark column. If `NULL`, uses `colname_ldk`.

- id:

  Integer. Index of point to become the first point. Default is 1.

- target:

  Numeric vector of length 2 (x, y). Find closest point to target.

- theta:

  Numeric. Angle in radians. Find point closest to this angle.

- direction:

  Character. Direction ("right", "up", "left", "down").

## Value

- If `x` is a single matrix: returns the matrix with reordered points

- If `x` is a list: returns a list of matrices with reordered points

- If `x` is a tibble: returns the tibble with specified coo columns
  reordered

## Details

- `coo_slide()`: slide to specific index

- `coo_slide_closest()`: slide to point closest to spatial target

- `coo_slide_angle()`: slide to point closest to angle direction

- `coo_slide_direction()`: slide to point in specified direction

- `coo_slide_gap()`: slide to largest gap in perimeter

These functions are landmark-aware: landmark indices are automatically
shifted (with wrapping) to match the new point order.
