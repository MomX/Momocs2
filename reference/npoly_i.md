# Inverse natural polynomial transform

Reconstruct curve coordinates from natural polynomial coefficients.

## Usage

``` r
npoly_i(x, nb_pts = 120, ..., .cols = NULL, .name = "_i")
```

## Arguments

- x:

  A numeric vector, list (from raw npoly), list of vectors, or tibble
  with coe columns.

- nb_pts:

  Integer. Number of points to reconstruct. Default is 120.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`, processes
  all coe columns automatically.

- .name:

  Character. Name suffix for output curve columns when `x` is a tibble.
  Default is `"_i"`. Output columns will be named `originalname_i`.

## Value

- If `x` is a vector or raw list: returns a matrix (nb_pts x 2) with
  class `"xy"`

- If `x` is a list of vectors: returns a list of matrices

- If `x` is a tibble: returns the tibble with new cur column(s) added

## Details

Performs inverse natural polynomial transform to reconstruct curve
coordinates from coefficients. The curve is reconstructed between
baseline points at (-0.5, 0) and (0.5, 0), then re-registered to
original baseline.

## See also

[`npoly()`](https://momx.github.io/Momocs2/reference/npoly.md) for
forward transform

## Examples

``` r
# Single curve
data(olea)
cur <- olea$VD[[1]]
coefs <- npoly(cur, degree = 6)

# Reconstruct
cur_reconstructed <- npoly_i(coefs)

# From raw output
coefs_raw <- npoly(cur, raw = TRUE)
npoly_i(coefs_raw)
#> <xy [120 x 2]>
#>       x      y     
#>  [1,] -0.500  0.000
#>  [2,] -0.492  0.014
#>  [3,] -0.484  0.028
#>  [4,] -0.475  0.042
#>  [5,] -0.467  0.054
#>  [6,] ...    ...   
#>  [7,] 0.466  0.056 
#>  [8,] 0.474  0.043 
#>  [9,] 0.483  0.029 
#> [10,] 0.491  0.015 
#> [11,] 0.500  0.000 

if (FALSE) { # \dontrun{
# Tibble workflow
library(dplyr)
olea %>%
  npoly(.cols = VD) %>%
  npoly_i()  # Creates VD_coe_i

# Custom column names
olea %>%
  npoly(.cols = VD) %>%
  npoly_i(.cols = VD_coe, .name = "VD_reconstructed")
} # }
```
