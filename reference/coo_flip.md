# Flip coordinates

Flip a shape along a specified axis or line.

## Usage

``` r
coo_flip_x(x, ..., .cols = NULL, .ldk_col = NULL)

coo_flip_xaxis(x, ..., .cols = NULL, .ldk_col = NULL)

coo_flip_y(x, ..., .cols = NULL, .ldk_col = NULL)

coo_flip_yaxis(x, ..., .cols = NULL, .ldk_col = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

- xintercept:

  Numeric. X intercept for flipping. Used in `coo_flip_x()`.

- yintercept:

  Numeric. Y intercept for flipping. Used in `coo_flip_y()`.

## Value

- If `x` is a single matrix: returns the flipped matrix

- If `x` is a list: returns a list of flipped matrices

- If `x` is a tibble: returns the tibble with specified coo columns
  flipped

## Details

- `coo_flip_x()`: flip across vertical line at x intercept

- `coo_flip_xaxis()`: flip across x-axis (y = 0)

- `coo_flip_y()`: flip across horizontal line at y intercept

- `coo_flip_yaxis()`: flip across y-axis (x = 0)

## See also

[`coo_rotate()`](https://momx.github.io/Momocs2/reference/coo_rotate.md)
for rotation;
[`coo_align()`](https://momx.github.io/Momocs2/reference/coo_align.md)
for alignment
