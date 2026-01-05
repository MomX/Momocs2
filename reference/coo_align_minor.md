# Align shape to minor and major axes (swapped)

Align a shape to its principal axes with major and minor axes swapped.

## Usage

``` r
coo_align_minor(x, ..., .cols = NULL, .ldk_col = NULL)
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

- If `x` is a single matrix: returns the aligned matrix

- If `x` is a list: returns a list of aligned matrices

- If `x` is a tibble: returns the tibble with specified coo columns
  aligned
