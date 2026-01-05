# Smooth coordinates

Smooth a shape using local averaging.

## Usage

``` r
coo_smooth(x, ..., .cols = NULL, .ldk_col = NULL)

coo_smooth_fixed(x, ..., .cols = NULL, .ldk_col = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

- n:

  Integer. Number of smoothing iterations. Default is 0 (no smoothing).

## Value

- If `x` is a single matrix: returns the smoothed matrix

- If `x` is a list: returns a list of smoothed matrices

- If `x` is a tibble: returns the tibble with specified coo columns
  smoothed

## Details

- `coo_smooth()`: smooth all points

- `coo_smooth_fixed()`: smooth while keeping first and last points fixed
