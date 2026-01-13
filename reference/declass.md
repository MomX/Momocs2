# Remove Momocs classes

Strip all Momocs-specific class attributes, retaining only base R
classes.

## Usage

``` r
declass(x)
```

## Arguments

- x:

  An object with Momocs classes.

## Value

Object with Momocs classes removed, retaining base R classes (e.g.,
"matrix", "numeric", "list", "data.frame").

## Details

Removes all Momocs morphometric classes while preserving base R
structure:

- Coordinate classes: `coo`, `out`, `ldk`, `cur`, `xy`

- Coefficient classes: `coe`, `eft`, `rft`, `dct`, `npoly`, `opoly`,
  `proc`

- Other classes: `ldk_id`, `path`, `meas`

This is useful when:

- Exporting data to other packages that don't recognize Momocs classes

- Debugging class-related issues

- Converting back to plain R objects for generic operations

The function works recursively on list-columns in data frames.

## See also

[as_class](https://momx.github.io/Momocs2/reference/as_class.md) for
adding Momocs classes

## Examples

``` r
# Single object
mat <- matrix(rnorm(100), ncol = 2)
outline <- as_out(mat)
class(outline)
#> [1] "out"    "coo"    "matrix" "array" 
# [1] "out"    "coo"    "matrix" "array"

plain <- declass(outline)
class(plain)
#> [1] "matrix" "array" 
# [1] "matrix" "array"

# Coefficient vector
coefs <- as_eft(rnorm(24))
class(coefs)
#> [1] "eft"     "coe"     "numeric"
# [1] "eft"     "coe"     "numeric"

plain_coefs <- declass(coefs)
class(plain_coefs)
#> [1] "numeric"
# [1] "numeric"

if (FALSE) { # \dontrun{
# Data frame with list-columns
library(dplyr)
df <- tibble(
  id = 1:3,
  shape = list(mat, mat, mat),
  coef = list(coefs, coefs, coefs)
) %>%
  mutate(
    shape = as_out(shape),
    coef = as_eft(coef)
  )

class(df$shape)
# [1] "out" "coo" "list"

df_plain <- declass(df)
class(df_plain$shape)
# [1] "list"
} # }
```
