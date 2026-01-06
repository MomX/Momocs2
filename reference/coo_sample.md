# Sample coordinates

Resample a shape to a different number of points.

## Usage

``` r
coo_sample(x, ..., .cols = NULL, .ldk_col = NULL)

coo_sample_prop(x, ..., .cols = NULL, .ldk_col = NULL)
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

- n:

  Integer. Target number of points. Minimum is 3.

- prop:

  Numeric. Proportion of points to keep (0 to 1).

## Value

- If `x` is a single matrix: returns the resampled matrix

- If `x` is a list: returns a list of resampled matrices

- If `x` is a tibble: returns the tibble with specified coo columns
  resampled

## Details

- `coo_sample()`: resample to exactly n points via arc-length
  interpolation

- `coo_sample_prop()`: resample to a proportion of original points

These functions are landmark-aware: if landmarks are present, the
outline is resampled segment-by-segment between landmarks, preserving
landmark positions and distributing points proportionally across
segments.

## See also

[`get_coords_nb()`](https://momx.github.io/Momocs2/reference/get_coords_nb.md)
for point count;
[`coo_smooth()`](https://momx.github.io/Momocs2/reference/coo_smooth.md)
for smoothing
