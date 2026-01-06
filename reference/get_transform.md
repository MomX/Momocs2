# Extract transformation between two configurations

Calculate the scale, rotation, and translation needed to transform one
configuration into another.

## Usage

``` r
get_transform(source, target)
```

## Arguments

- source:

  Matrix (nx2). Source configuration.

- target:

  Matrix (nx2). Target configuration.

## Value

A list with three elements:

- `scale`: Numeric. Scale factor.

- `rotation`: Numeric. Rotation angle in radians.

- `translation`: Numeric vector of length 2. Translation (dx, dy).

## Details

Calculates transformation based on the furthest pair of points in the
source configuration. Finds the corresponding pair in the target and
extracts:

- Scale: ratio of distances between furthest points

- Rotation: angle difference between the two vectors

- Translation: shift after scaling and rotation

Both configurations must have the same number of points.

## See also

[`get_transformed()`](https://momx.github.io/Momocs2/reference/get_transformed.md)

## Examples

``` r
source <- matrix(c(0,0, 1,0, 1,1, 0,1), ncol=2, byrow=TRUE)
target <- source * 2  # Scaled by 2
transform <- get_transform(source, target)
transform$scale  # Should be ~2
#> [1] 2
```
