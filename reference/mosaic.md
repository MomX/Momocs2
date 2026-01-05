# Plot shapes in grid mosaic layout

Display one or more shapes arranged in a grid layout with automatic
positioning.

## Usage

``` r
mosaic(x, ratio = NULL, nrow = NULL, ncol = NULL, ..., .cols = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ratio:

  Numeric. Target aspect ratio (width/height) for the mosaic. If NULL
  and nrow/ncol not specified, creates square-ish layout.

- nrow:

  Integer. Number of rows in the grid.

- ncol:

  Integer. Number of columns in the grid.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

## Value

Invisibly returns list of translated matrices (for piping).

## Details

Arranges shapes in a grid mosaic. Shapes are assumed to be templated
(centered in a 1x1 bounding box). Works with:

- Single matrix: plots that shape

- List of matrices: arranges all shapes in grid

- Tibble: arranges all shapes from coo columns (or specified `.cols`)

Grid dimensions can be controlled via `nrow`, `ncol`, or `ratio`. If
none are specified, creates a square-ish layout.

## Examples

``` r
mosaic(shapes$cat)

mosaic(shapes)

mosaic(bot)

mosaic(shapes, nrow = 2)

mosaic(shapes, ratio = 16/9)
```
