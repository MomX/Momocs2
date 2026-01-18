# Discrete Cosine Transform

Compute discrete cosine transform coefficients from open curve
coordinates.

## Usage

``` r
dct(x, nb_h = 12, raw = FALSE, ..., .cols = NULL, .name = "_coe")
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- nb_h:

  Integer. Number of harmonics to compute. Default is 12.

- raw:

  Logical. If `TRUE`, returns a list with A, B coefficients and
  additional components (only for single matrix). Default is `FALSE`.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`, processes
  all coo columns automatically.

- .name:

  Character. Name suffix for output coefficient columns when `x` is a
  tibble. Default is `"_coe"`. Output columns will be named
  `originalname_coe`. If a single custom name is provided and multiple
  columns are processed, an error is raised.

## Value

- If `x` is a single matrix and `raw = FALSE`: returns a named numeric
  vector with classes `c("dct", "numeric")` containing 2\*nb_h
  coefficients (A1-An, B1-Bn)

- If `x` is a single matrix and `raw = TRUE`: returns a list with
  elements `an`, `bn`, `mod`, `arg`

- If `x` is a list: returns a list of coefficient vectors (each with
  class `c("dct", "numeric")`)

- If `x` is a tibble: returns the tibble with new list-column(s) of
  class `c("dct", "coe", "list")` containing coefficient vectors

## Details

The Discrete Cosine Transform (DCT) is a Fourier-related method for
analyzing open contours. It decomposes curves into a sum of cosine
functions with different frequencies, capturing shape variation
efficiently.

For each harmonic n, two coefficients are computed:

- An: Real part (related to x-coordinates)

- Bn: Imaginary part (related to y-coordinates)

The method also computes modulus and argument for each harmonic when
`raw = TRUE`:

- mod: Amplitude of each harmonic

- arg: Phase angle of each harmonic

The curve is automatically baseline-normalized using the first and last
points as landmarks, positioned at (-0.5, 0) and (0.5, 0) respectively.

### Choosing the number of harmonics

The default `nb_h = 12` works well for most open curves. More harmonics
capture finer details but increase dimensionality. Unlike polynomial
methods, DCT harmonics are independent and don't change when adding
more.

## Note

This method can be computationally intensive for large datasets. The
implementation is optimized but may still take time for many shapes.

## References

Dommergues, C. H., Dommergues, J.-L., & Verrecchia, E. P. (2007). The
Discrete Cosine Transform, a Fourier-related Method for Morphometric
Analysis of Open Contours. *Mathematical Geology*, 39(8), 749-763.

## See also

[`dct_i()`](https://momx.github.io/Momocs2/reference/dct_i.md) for
inverse transform,
[`opoly()`](https://momx.github.io/Momocs2/reference/opoly.md),
[`npoly()`](https://momx.github.io/Momocs2/reference/npoly.md) for
polynomial methods.

## Examples

``` r
# Single curve
data(olea)
cur <- olea$VD[[1]]
dct_coefs <- dct(cur)
dct_coefs
#>           A1           A2           A3           A4           A5           A6 
#> -3.118207297 -0.132067147 -0.247813902 -0.096603251 -0.067883107 -0.066911685 
#>           A7           A8           A9          A10          A11          A12 
#> -0.035197194 -0.060161203 -0.020710016 -0.065449940 -0.011697040 -0.068107219 
#>           B1           B2           B3           B4           B5           B6 
#>  0.032926049 -0.914830858  0.005334948 -0.268975696 -0.006644877 -0.101625518 
#>           B7           B8           B9          B10          B11          B12 
#>  0.003834764 -0.049467452  0.003042230 -0.028964333 -0.002260202 -0.022833346 
#> attr(,"class")
#> [1] "dct"     "numeric"

# Get raw output with modulus and argument
dct(cur, raw = TRUE)
#> $an
#>  [1] -3.11820730 -0.13206715 -0.24781390 -0.09660325 -0.06788311 -0.06691169
#>  [7] -0.03519719 -0.06016120 -0.02071002 -0.06544994 -0.01169704 -0.06810722
#> 
#> $bn
#>  [1]  0.032926049 -0.914830858  0.005334948 -0.268975696 -0.006644877
#>  [6] -0.101625518  0.003834764 -0.049467452  0.003042230 -0.028964333
#> [11] -0.002260202 -0.022833346
#> 
#> $mod
#>  [1] 3.11838113 0.92431446 0.24787132 0.28579733 0.06820756 0.12167547
#>  [7] 0.03540548 0.07788709 0.02093227 0.07157253 0.01191341 0.07183283
#> 
#> $arg
#>  [1]  3.131034 -1.714168  3.120068 -1.915601 -3.044016 -2.153064  3.033070
#>  [8] -2.453432  2.995739 -2.724958 -2.950716 -2.818113
#> 
#> $baseline1
#> [1] -0.5  0.0
#> 
#> $baseline2
#> [1] 0.5 0.0
#> 

# With more harmonics
dct(cur, nb_h = 15)
#>           A1           A2           A3           A4           A5           A6 
#> -3.118207297 -0.132067147 -0.247813902 -0.096603251 -0.067883107 -0.066911685 
#>           A7           A8           A9          A10          A11          A12 
#> -0.035197194 -0.060161203 -0.020710016 -0.065449940 -0.011697040 -0.068107219 
#>          A13          A14          A15           B1           B2           B3 
#> -0.009012298 -0.068385621 -0.005665874  0.032926049 -0.914830858  0.005334948 
#>           B4           B5           B6           B7           B8           B9 
#> -0.268975696 -0.006644877 -0.101625518  0.003834764 -0.049467452  0.003042230 
#>          B10          B11          B12          B13          B14          B15 
#> -0.028964333 -0.002260202 -0.022833346 -0.003430171 -0.015162555  0.003848665 
#> attr(,"class")
#> [1] "dct"     "numeric"

# List of curves
cur_list <- list(cur, cur * 1.5, cur * 2)
dct(cur_list)
#> [[1]]
#>           A1           A2           A3           A4           A5           A6 
#> -3.118207297 -0.132067147 -0.247813902 -0.096603251 -0.067883107 -0.066911685 
#>           A7           A8           A9          A10          A11          A12 
#> -0.035197194 -0.060161203 -0.020710016 -0.065449940 -0.011697040 -0.068107219 
#>           B1           B2           B3           B4           B5           B6 
#>  0.032926049 -0.914830858  0.005334948 -0.268975696 -0.006644877 -0.101625518 
#>           B7           B8           B9          B10          B11          B12 
#>  0.003834764 -0.049467452  0.003042230 -0.028964333 -0.002260202 -0.022833346 
#> attr(,"class")
#> [1] "dct"     "numeric"
#> 
#> [[2]]
#>           A1           A2           A3           A4           A5           A6 
#> -3.118207297 -0.132067147 -0.247813902 -0.096603251 -0.067883107 -0.066911685 
#>           A7           A8           A9          A10          A11          A12 
#> -0.035197194 -0.060161203 -0.020710016 -0.065449940 -0.011697040 -0.068107219 
#>           B1           B2           B3           B4           B5           B6 
#>  0.032926049 -0.914830858  0.005334948 -0.268975696 -0.006644877 -0.101625518 
#>           B7           B8           B9          B10          B11          B12 
#>  0.003834764 -0.049467452  0.003042230 -0.028964333 -0.002260202 -0.022833346 
#> attr(,"class")
#> [1] "dct"     "numeric"
#> 
#> [[3]]
#>           A1           A2           A3           A4           A5           A6 
#> -3.118207297 -0.132067147 -0.247813902 -0.096603251 -0.067883107 -0.066911685 
#>           A7           A8           A9          A10          A11          A12 
#> -0.035197194 -0.060161203 -0.020710016 -0.065449940 -0.011697040 -0.068107219 
#>           B1           B2           B3           B4           B5           B6 
#>  0.032926049 -0.914830858  0.005334948 -0.268975696 -0.006644877 -0.101625518 
#>           B7           B8           B9          B10          B11          B12 
#>  0.003834764 -0.049467452  0.003042230 -0.028964333 -0.002260202 -0.022833346 
#> attr(,"class")
#> [1] "dct"     "numeric"
#> 
#> attr(,"class")
#> [1] "dct"  "coe"  "list"

if (FALSE) { # \dontrun{
# Tibble - processes all coo columns by default
library(dplyr)
olea %>%
  dct(nb_h = 10)  # Creates VD_coe and VL_coe

# Process specific column only
olea %>%
  dct(.cols = VD)

# Custom name for single column
olea %>%
  dct(.cols = VL, .name = "dct_coeffs")
} # }
```
