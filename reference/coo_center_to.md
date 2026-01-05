# Center and translate coordinates

Center a shape and then translate it to specified coordinates.

## Usage

``` r
coo_center_to(x, ..., .cols = NULL, .ldk_col = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

- x_val:

  Numeric. X coordinate to translate to. Default is 0.

- y_val:

  Numeric. Y coordinate to translate to. Default is 0.

## Value

- If `x` is a single matrix: returns the centered and translated matrix

- If `x` is a list: returns a list of centered and translated matrices

- If `x` is a tibble: returns the tibble with specified coo columns
  transformed
