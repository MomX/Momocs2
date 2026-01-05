# Translate shape to x-axis

Translate a shape so its centroid aligns with the x-axis.

## Usage

``` r
coo_translate_to_xaxis(x, ..., .cols = NULL, .ldk_col = NULL)
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

- If `x` is a single matrix: returns the transformed matrix

- If `x` is a list: returns a list of transformed matrices

- If `x` is a tibble: returns the tibble with specified coo columns
  transformed
