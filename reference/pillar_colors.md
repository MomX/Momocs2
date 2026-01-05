# Customize pillar colors

Set custom colors for morphometric data types in tibble printing.

## Arguments

- ...:

  Named arguments where names are class names and values are color
  names. Available colors: "blue", "red", "green", "yellow", "cyan",
  "magenta", "silver", "grey"

## Details

Colors can be set individually using options:

- `options(momocs2.pillar.color.out = "blue")`

- `options(momocs2.pillar.color.ldk = "red")`

- `options(momocs2.pillar.color.ldk_id = "red")`

- `options(momocs2.pillar.color.coo = "blue")`

- `options(momocs2.pillar.color.cur = "cyan")`

- `options(momocs2.pillar.color.dct = "cyan")`

- `options(momocs2.pillar.color.eft = "cyan")`

- `options(momocs2.pillar.color.npoly = "cyan")`

- `options(momocs2.pillar.color.opoly = "cyan")`

- `options(momocs2.pillar.color.proc = "green")`

- `options(momocs2.pillar.color.path = "silver")`

Default colors:

- Coordinates: blue (out, coo)

- Landmarks: red (ldk, ldk_id)

- Coefficients: cyan (cur, dct, eft, npoly, opoly)

- Procrustes: green (proc)

- Paths: silver (path)

## Examples

``` r
if (FALSE) { # \dontrun{
# Set all landmarks to magenta
options(momocs2.pillar.color.ldk = "magenta")
options(momocs2.pillar.color.ldk_id = "magenta")

# Set outlines to green
options(momocs2.pillar.color.out = "green")

# Reset to defaults (restart R or set NULL)
options(momocs2.pillar.color.out = NULL)
} # }
```
