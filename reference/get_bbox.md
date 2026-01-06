# Get bounding box corners

Calculate the four corners of the bounding box.

## Usage

``` r
get_bbox(x, ..., .cols = NULL)
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

- If `x` is a single matrix: returns a 4x2 matrix with corners

- If `x` is a list: returns a list of 4x2 matrices

- If `x` is a tibble: returns a list of 4x2 matrices extracted from coo
  column

## Details

Returns the four corners of the axis-aligned bounding box as a 4x2
matrix:

- Row 1: bottom-left (xmin, ymin)

- Row 2: bottom-right (xmax, ymin)

- Row 3: top-right (xmax, ymax)

- Row 4: top-left (xmin, ymax)

This forms a closed rectangle suitable for plotting. This is not a
scalar, so cannot be used with
[`measure()`](https://momx.github.io/Momocs2/reference/measure.md).

## Examples

``` r
get_bbox(shapes$cat)
#>      x   y
#> bl 130   9
#> br 249   9
#> tr 249 231
#> tl 130 231

# Plot shape with bounding box
p(shapes$cat)
draw_outlines(list(shapes$cat))
draw_outlines(list(get_bbox(shapes$cat)), col = "red")

```
