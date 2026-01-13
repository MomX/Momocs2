# Relocate morphometric columns to front

Reorder columns in a tibble so that morphometric data columns appear
first in a logical order: path columns, then coo columns, then coe
columns, followed by all other columns.

## Usage

``` r
front(.data)
```

## Arguments

- .data:

  A tibble or data frame

## Value

The same tibble with columns reordered

## Details

This is a convenience function to organize morphometric data in a
consistent, readable order. The ordering priority is:

1.  **path** columns (image file paths) - typically named "path" or
    containing "path"

2.  **coo** columns (coordinates) - list-columns with class "coo"

3.  **coe** columns (coefficients) - list-columns with class "coe"

4.  **everything else** - metadata, grouping variables, etc.

If multiple columns of the same type exist, their relative order is
preserved.

## Examples

``` r
if (FALSE) { # \dontrun{
# After adding coefficients, coe column is at the end
bot %>% efourier()

# Relocate to put coe after coo
bot %>% efourier() %>% front()

# Works with any combination
tibble(
  id = 1:3,
  species = c("A", "B", "C"),
  coe = list(1:24, 1:24, 1:24),
  coo = list(matrix(1:10, ncol=2), matrix(1:10, ncol=2), matrix(1:10, ncol=2)),
  path = c("img1.jpg", "img2.jpg", "img3.jpg")
) %>% front()
# Result: path, coo, coe, id, species
} # }
```
