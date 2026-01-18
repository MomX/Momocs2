# Elliptic Fourier Transform

Compute elliptic Fourier transform coefficients from outline
coordinates.

## Usage

``` r
eft(x, nb_h = 6, raw = FALSE, ..., .cols = NULL, .name = "coe")
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- nb_h:

  Integer. Number of harmonics to compute. Default is 6.

- raw:

  Logical. If `TRUE`, returns a list with a0, c0 and harmonic
  coefficients (only for single matrix). Default is `FALSE`.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects. If multiple coo
  columns exist, must be specified explicitly.

- .name:

  Character. Name for the output coefficient column when `x` is a
  tibble. Default is `"coe"`. If this column already exists, an error is
  raised.

## Value

- If `x` is a single matrix and `raw = FALSE`: returns a named numeric
  vector with classes `c("eft", "numeric")` containing 4\*nb_h
  coefficients (An, Bn, Cn, Dn)

- If `x` is a single matrix and `raw = TRUE`: returns a list with
  elements `an`, `bn`, `cn`, `dn`, `a0`, `c0`

- If `x` is a list: returns a list of coefficient vectors (each with
  class `c("eft", "numeric")`)

- If `x` is a tibble: returns the tibble with a new list-column of class
  `c("eft", "coe", "list")` containing coefficient vectors

## Details

Elliptic Fourier analysis decomposes a closed outline into a sum of
harmonically related ellipses. The method is based on separate Fourier
decompositions of the incremental changes in x and y coordinates as a
function of cumulative perimeter distance.

For each harmonic n, four coefficients are computed:

- An, Bn: coefficients for x-coordinates (cosine and sine terms)

- Cn, Dn: coefficients for y-coordinates (cosine and sine terms)

The `raw = TRUE` option additionally returns `a0` and `c0` (the constant
terms), which represent the centroid position. This is only available
for single matrices.

### Choosing the number of harmonics

The default `nb_h = 6` works well for most outlines. More harmonics
capture finer details but increase dimensionality. Use fewer harmonics
for simpler shapes or to reduce noise. As a rule of thumb, nb_h should
be less than the number of outline points divided by 3.

## See also

[`eft_i()`](https://momx.github.io/Momocs2/reference/eft_i.md) for
inverse transform,
[`eft_norm()`](https://momx.github.io/Momocs2/reference/eft_norm.md) for
numeric normalisation.

## Examples

``` r
# Single outline
coo <- matrix(runif(100), ncol = 2)
eft_coefs <- eft(coo)
eft_coefs
#>           A1           A2           A3           A4           A5           A6 
#> -0.010397912  0.027576921  0.100043703  0.030489295 -0.053189126 -0.042869106 
#>           B1           B2           B3           B4           B5           B6 
#>  0.060434775 -0.037498977  0.041570983 -0.091228495  0.007018227 -0.005718215 
#>           C1           C2           C3           C4           C5           C6 
#> -0.033337897 -0.007007593 -0.028361017 -0.051594046 -0.026691922 -0.050881116 
#>           D1           D2           D3           D4           D5           D6 
#>  0.052039575 -0.014615057 -0.049919414  0.007118126  0.043556980  0.006276565 
#> attr(,"class")
#> [1] "eft"     "numeric"

# Get raw output with a0 and c0
eft(coo, raw = TRUE)
#> $an
#> [1] -0.01039791  0.02757692  0.10004370  0.03048929 -0.05318913 -0.04286911
#> 
#> $bn
#> [1]  0.060434775 -0.037498977  0.041570983 -0.091228495  0.007018227
#> [6] -0.005718215
#> 
#> $cn
#> [1] -0.033337897 -0.007007593 -0.028361017 -0.051594046 -0.026691922
#> [6] -0.050881116
#> 
#> $dn
#> [1]  0.052039575 -0.014615057 -0.049919414  0.007118126  0.043556980
#> [6]  0.006276565
#> 
#> $a0
#> [1] 1.042652
#> 
#> $c0
#> [1] 1.003171
#> 

# With more harmonics
eft(coo, nb_h = 10)
#>           A1           A2           A3           A4           A5           A6 
#> -0.010397912  0.027576921  0.100043703  0.030489295 -0.053189126 -0.042869106 
#>           A7           A8           A9          A10           B1           B2 
#>  0.061658314  0.101091076  0.016094072  0.050629043  0.060434775 -0.037498977 
#>           B3           B4           B5           B6           B7           B8 
#>  0.041570983 -0.091228495  0.007018227 -0.005718215  0.029820930  0.064012872 
#>           B9          B10           C1           C2           C3           C4 
#>  0.043297514 -0.016803976 -0.033337897 -0.007007593 -0.028361017 -0.051594046 
#>           C5           C6           C7           C8           C9          C10 
#> -0.026691922 -0.050881116 -0.016578719  0.034711700  0.021383473  0.069054253 
#>           D1           D2           D3           D4           D5           D6 
#>  0.052039575 -0.014615057 -0.049919414  0.007118126  0.043556980  0.006276565 
#>           D7           D8           D9          D10 
#> -0.006709972  0.030305394  0.051649070 -0.009370523 
#> attr(,"class")
#> [1] "eft"     "numeric"

# List of outlines
coo_list <- list(coo, coo * 1.5, coo * 2)
eft(coo_list)
#> [[1]]
#>           A1           A2           A3           A4           A5           A6 
#> -0.010397912  0.027576921  0.100043703  0.030489295 -0.053189126 -0.042869106 
#>           B1           B2           B3           B4           B5           B6 
#>  0.060434775 -0.037498977  0.041570983 -0.091228495  0.007018227 -0.005718215 
#>           C1           C2           C3           C4           C5           C6 
#> -0.033337897 -0.007007593 -0.028361017 -0.051594046 -0.026691922 -0.050881116 
#>           D1           D2           D3           D4           D5           D6 
#>  0.052039575 -0.014615057 -0.049919414  0.007118126  0.043556980  0.006276565 
#> attr(,"class")
#> [1] "eft"     "numeric"
#> 
#> [[2]]
#>           A1           A2           A3           A4           A5           A6 
#> -0.015596869  0.041365381  0.150065554  0.045733942 -0.079783689 -0.064303659 
#>           B1           B2           B3           B4           B5           B6 
#>  0.090652163 -0.056248465  0.062356474 -0.136842743  0.010527340 -0.008577323 
#>           C1           C2           C3           C4           C5           C6 
#> -0.050006845 -0.010511389 -0.042541526 -0.077391069 -0.040037883 -0.076321674 
#>           D1           D2           D3           D4           D5           D6 
#>  0.078059362 -0.021922586 -0.074879122  0.010677190  0.065335469  0.009414847 
#> attr(,"class")
#> [1] "eft"     "numeric"
#> 
#> [[3]]
#>          A1          A2          A3          A4          A5          A6 
#> -0.02079582  0.05515384  0.20008741  0.06097859 -0.10637825 -0.08573821 
#>          B1          B2          B3          B4          B5          B6 
#>  0.12086955 -0.07499795  0.08314197 -0.18245699  0.01403645 -0.01143643 
#>          C1          C2          C3          C4          C5          C6 
#> -0.06667579 -0.01401519 -0.05672203 -0.10318809 -0.05338384 -0.10176223 
#>          D1          D2          D3          D4          D5          D6 
#>  0.10407915 -0.02923011 -0.09983883  0.01423625  0.08711396  0.01255313 
#> attr(,"class")
#> [1] "eft"     "numeric"
#> 
#> attr(,"class")
#> [1] "eft"  "coe"  "list"

if (FALSE) { # \dontrun{
# Tibble (requires coo column)
library(dplyr)
bot %>%
  eft(nb_h = 8)

# Specify column explicitly
bot %>%
  eft(.cols = coo, .name = "eft_coefs")
} # }
```
