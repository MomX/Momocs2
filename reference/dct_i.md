# Inverse discrete cosine transform

Reconstruct curve coordinates from discrete cosine transform
coefficients.

## Usage

``` r
dct_i(x, nb_h = NULL, nb_pts = 60, ..., .cols = NULL, .name = "_i")
```

## Arguments

- x:

  A numeric vector, list (from raw dct), list of vectors, or tibble with
  coe columns.

- nb_h:

  Integer. Number of harmonics to use for reconstruction. If `NULL`,
  uses all available harmonics.

- nb_pts:

  Integer. Number of points to reconstruct. Default is 60.

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

Performs inverse discrete cosine transform to reconstruct curve
coordinates from coefficients. This is useful for:

- Visualizing shapes at different harmonic levels

- Reconstructing shapes from PCA or other statistical analyses

- Filtering out high-frequency noise by using fewer harmonics

If `nb_h` is less than the total number of harmonics in the
coefficients, only the first `nb_h` harmonics are used (low-pass
filtering).

## See also

[`dct()`](https://momx.github.io/Momocs2/reference/dct.md) for forward
transform

## Examples

``` r
# Single curve
data(olea)
cur <- olea$VD[[1]]
coefs <- dct(cur, nb_h = 12)

# Reconstruct with all harmonics
cur_reconstructed <- dct_i(coefs)

# Reconstruct with fewer harmonics (smoothing)
cur_smooth <- dct_i(coefs, nb_h = 6)

# From raw output
coefs_raw <- dct(cur, raw = TRUE)
dct_i(coefs_raw)
#> <xy [60 x 2]>
#>       x      y     
#>  [1,] -0.500  0.000
#>  [2,] -0.486  0.008
#>  [3,] -0.462  0.023
#>  [4,] -0.432  0.043
#>  [5,] -0.401  0.065
#>  [6,] ...    ...   
#>  [7,] 0.521  0.065 
#>  [8,] 0.518  0.043 
#>  [9,] 0.512  0.023 
#> [10,] 0.505  0.008 
#> [11,] 0.500  0.000 

if (FALSE) { # \dontrun{
# Tibble workflow
library(dplyr)
olea %>%
  dct(.cols = VD) %>%
  dct_i()  # Creates VD_coe_i

# Custom column names
olea %>%
  dct(.cols = VD) %>%
  dct_i(.cols = VD_coe, .name = "VD_reconstructed")
} # }
```
