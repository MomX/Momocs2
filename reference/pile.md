# Plot shapes stacked together

Display one or more shapes stacked on the same plot.

## Usage

``` r
pile(x, ..., .cols = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments passed to plotting functions.

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

## Value

Invisibly returns `x` (for piping).

## Details

Stacks all shapes on a single plot. Works with:

- Single matrix: plots that shape

- List of matrices: plots all shapes overlaid

- Tibble: plots all coo columns (or specified `.cols`)

## Examples

``` r
pile(shapes$cat)

pile(shapes)

pile(bot)

pile(bot, .cols = "coo")
```
