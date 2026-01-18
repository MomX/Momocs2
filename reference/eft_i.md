# Inverse elliptic Fourier transform

Reconstruct outline coordinates from elliptic Fourier coefficients.

## Usage

``` r
eft_i(x, nb_h = NULL, nb_pts = 120, ..., .cols = NULL, .name = NULL)
```

## Arguments

- x:

  A numeric vector, list (from raw eft), list of vectors, or tibble with
  coe columns.

- nb_h:

  Integer. Number of harmonics to use for reconstruction. If `NULL`,
  uses all available harmonics.

- nb_pts:

  Integer. Number of points to reconstruct. Default is 120.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coe objects.

- .name:

  Character. Name for the output coo column when `x` is a tibble. If
  `NULL`, uses the pattern `"colname_i"` (e.g., `"coe_i"`).

## Value

- If `x` is a vector or raw list: returns a matrix (nb_pts x 2) with
  class `"xy"`

- If `x` is a list of vectors: returns a list of matrices

- If `x` is a tibble: returns the tibble with a new coo column added

## Details

Performs inverse elliptic Fourier transform to reconstruct outline
coordinates from coefficients. This is useful for:

- Visualizing shapes at different harmonic levels

- Reconstructing shapes from PCA or other statistical analyses

- Filtering out high-frequency noise by using fewer harmonics

If `nb_h` is less than the total number of harmonics in the
coefficients, only the first `nb_h` harmonics are used (low-pass
filtering).

## See also

[`eft()`](https://momx.github.io/Momocs2/reference/eft.md) for forward
transform

## Examples

``` r
# Single outline
coo <- matrix(runif(100), ncol = 2)
coefs <- eft(coo, nb_h = 6)

# Reconstruct with all harmonics
coo_reconstructed <- eft_i(coefs)

# Reconstruct with fewer harmonics (smoothing)
coo_smooth <- eft_i(coefs, nb_h = 3)

# From raw output
coefs_raw <- eft(coo, raw = TRUE)
eft_i(coefs_raw)
#> <xy [120 x 2]>
#>       x     y    
#>  [1,] 0.517 0.490
#>  [2,] 0.513 0.506
#>  [3,] 0.507 0.515
#>  [4,] 0.500 0.520
#>  [5,] 0.490 0.520
#>  [6,] ...   ...  
#>  [7,] 0.533 0.365
#>  [8,] 0.529 0.390
#>  [9,] 0.526 0.417
#> [10,] 0.523 0.444
#> [11,] 0.520 0.469

if (FALSE) { # \dontrun{
# Tibble workflow
library(dplyr)
bot %>%
  eft() %>%
  eft_i()

# Custom column names
bot %>%
  eft(.name = "eft") %>%
  eft_i(.cols = eft, .name = "coo_reconstructed")
} # }
```
