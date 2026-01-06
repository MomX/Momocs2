# Apply transformation to coordinates

Apply a transformation (scale, rotation, translation) to shape
coordinates.

## Usage

``` r
get_transformed(x, ..., .cols = NULL, .ldk_col = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo columns.

- .ldk_col:

  Character. Name of landmark column. If `NULL`, uses `colname_ldk`.

- transform:

  A list with elements `scale`, `rotation`, and `translation`, typically
  from
  [`get_transform()`](https://momx.github.io/Momocs2/reference/get_transform.md).

## Value

- If `x` is a single matrix: returns the transformed matrix

- If `x` is a list: returns a list of transformed matrices

- If `x` is a tibble: returns the tibble with transformed coo column(s)

## Details

Applies transformation in order:

1.  Scale (multiply coordinates by scale factor)

2.  Rotate (by rotation angle in radians)

3.  Translate (shift by translation vector)

Landmarks are transformed along with coordinates.

## See also

[`get_transform()`](https://momx.github.io/Momocs2/reference/get_transform.md)

## Examples

``` r
source <- matrix(c(0,0, 1,0, 1,1, 0,1), ncol=2, byrow=TRUE)
target <- source * 2  # Scaled by 2

# Extract transformation
transform <- get_transform(source, target)

# Apply to another shape
new_shape <- matrix(c(0,0, 2,0, 2,2), ncol=2, byrow=TRUE)
transformed <- get_transformed(new_shape, transform)
```
