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
#>            A1            A2            A3            A4            A5 
#> -0.0531428944  0.0196295077  0.0470383236  0.0468602674  0.0085574396 
#>            A6            B1            B2            B3            B4 
#> -0.0574998094 -0.0356972021 -0.0100539936 -0.0633199157  0.0396765031 
#>            B5            B6            C1            C2            C3 
#>  0.0386983870  0.0576810225 -0.0611678439  0.0371348396 -0.0210267491 
#>            C4            C5            C6            D1            D2 
#>  0.0076610408  0.0461152002  0.0005502462  0.0463335920  0.0349596177 
#>            D3            D4            D5            D6 
#> -0.0503341655 -0.0333677007 -0.0303897742  0.0459143693 
#> attr(,"class")
#> [1] "eft"     "numeric"

# Get raw output with a0 and c0
eft(coo, raw = TRUE)
#> $an
#> [1] -0.05314289  0.01962951  0.04703832  0.04686027  0.00855744 -0.05749981
#> 
#> $bn
#> [1] -0.03569720 -0.01005399 -0.06331992  0.03967650  0.03869839  0.05768102
#> 
#> $cn
#> [1] -0.0611678439  0.0371348396 -0.0210267491  0.0076610408  0.0461152002
#> [6]  0.0005502462
#> 
#> $dn
#> [1]  0.04633359  0.03495962 -0.05033417 -0.03336770 -0.03038977  0.04591437
#> 
#> $a0
#> [1] 1.034815
#> 
#> $c0
#> [1] 0.907308
#> 

# With more harmonics
eft(coo, nb_h = 10)
#>            A1            A2            A3            A4            A5 
#> -0.0531428944  0.0196295077  0.0470383236  0.0468602674  0.0085574396 
#>            A6            A7            A8            A9           A10 
#> -0.0574998094 -0.0645760964 -0.0679580910  0.0020245103 -0.0139025880 
#>            B1            B2            B3            B4            B5 
#> -0.0356972021 -0.0100539936 -0.0633199157  0.0396765031  0.0386983870 
#>            B6            B7            B8            B9           B10 
#>  0.0576810225  0.0051253066 -0.0115141086 -0.0053056912  0.0565203843 
#>            C1            C2            C3            C4            C5 
#> -0.0611678439  0.0371348396 -0.0210267491  0.0076610408  0.0461152002 
#>            C6            C7            C8            C9           C10 
#>  0.0005502462 -0.0658095959  0.1062392666  0.0445585065  0.0283524298 
#>            D1            D2            D3            D4            D5 
#>  0.0463335920  0.0349596177 -0.0503341655 -0.0333677007 -0.0303897742 
#>            D6            D7            D8            D9           D10 
#>  0.0459143693 -0.0241751489 -0.0103659488 -0.0096842865  0.0122717238 
#> attr(,"class")
#> [1] "eft"     "numeric"

# List of outlines
coo_list <- list(coo, coo * 1.5, coo * 2)
eft(coo_list)
#> [[1]]
#>            A1            A2            A3            A4            A5 
#> -0.0531428944  0.0196295077  0.0470383236  0.0468602674  0.0085574396 
#>            A6            B1            B2            B3            B4 
#> -0.0574998094 -0.0356972021 -0.0100539936 -0.0633199157  0.0396765031 
#>            B5            B6            C1            C2            C3 
#>  0.0386983870  0.0576810225 -0.0611678439  0.0371348396 -0.0210267491 
#>            C4            C5            C6            D1            D2 
#>  0.0076610408  0.0461152002  0.0005502462  0.0463335920  0.0349596177 
#>            D3            D4            D5            D6 
#> -0.0503341655 -0.0333677007 -0.0303897742  0.0459143693 
#> attr(,"class")
#> [1] "eft"     "numeric"
#> 
#> [[2]]
#>            A1            A2            A3            A4            A5 
#> -0.0797143415  0.0294442615  0.0705574854  0.0702904011  0.0128361593 
#>            A6            B1            B2            B3            B4 
#> -0.0862497141 -0.0535458031 -0.0150809904 -0.0949798736  0.0595147546 
#>            B5            B6            C1            C2            C3 
#>  0.0580475805  0.0865215338 -0.0917517658  0.0557022593 -0.0315401237 
#>            C4            C5            C6            D1            D2 
#>  0.0114915611  0.0691728004  0.0008253694  0.0695003879  0.0524394266 
#>            D3            D4            D5            D6 
#> -0.0755012482 -0.0500515510 -0.0455846613  0.0688715540 
#> attr(,"class")
#> [1] "eft"     "numeric"
#> 
#> [[3]]
#>           A1           A2           A3           A4           A5           A6 
#> -0.106285789  0.039259015  0.094076647  0.093720535  0.017114879 -0.114999619 
#>           B1           B2           B3           B4           B5           B6 
#> -0.071394404 -0.020107987 -0.126639831  0.079353006  0.077396774  0.115362045 
#>           C1           C2           C3           C4           C5           C6 
#> -0.122335688  0.074269679 -0.042053498  0.015322082  0.092230400  0.001100492 
#>           D1           D2           D3           D4           D5           D6 
#>  0.092667184  0.069919235 -0.100668331 -0.066735401 -0.060779548  0.091828739 
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
