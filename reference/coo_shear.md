# Shear coordinates

Apply shear transformation to a shape.

## Usage

``` r
coo_shear_x(x, ..., .cols = NULL, .ldk_col = NULL)

coo_shear_y(x, ..., .cols = NULL, .ldk_col = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

- k:

  Numeric. Shear factor. Default is 1.

## Value

- If `x` is a single matrix: returns the sheared matrix

- If `x` is a list: returns a list of sheared matrices

- If `x` is a tibble: returns the tibble with specified coo columns
  sheared

## Details

- `coo_shear_x()`: shear in x direction

- `coo_shear_y()`: shear in y direction

## See also

[`coo_rotate()`](https://momx.github.io/Momocs2/reference/coo_rotate.md)
for rotation;
[`coo_flip()`](https://momx.github.io/Momocs2/reference/coo_flip.md) for
flipping
