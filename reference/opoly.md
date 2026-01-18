# Orthogonal Polynomial Transform

Compute orthogonal (Legendre) polynomial coefficients from open curve
coordinates.

## Usage

``` r
opoly(x, degree = 5, raw = FALSE, ..., .cols = NULL, .name = "_coe")
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- degree:

  Integer. Polynomial degree for the fit. Default is 5.

- raw:

  Logical. If `TRUE`, returns a list with coefficients and fit details
  (only for single matrix). Default is `FALSE`.

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
  vector with classes `c("opoly", "numeric")` containing degree+1
  coefficients (x0, x1, ..., xN)

- If `x` is a single matrix and `raw = TRUE`: returns a list with
  elements `coeff`, `degree`, `baseline1`, `baseline2`, `r2`, `mod`

- If `x` is a list: returns a list of coefficient vectors (each with
  class `c("opoly", "numeric")`)

- If `x` is a tibble: returns the tibble with new list-column(s) of
  class `c("opoly", "coe", "list")` containing coefficient vectors

## Details

Orthogonal polynomial fitting uses Legendre polynomials to decompose
open curves. The method fits a polynomial model to the y-coordinates as
a function of x-coordinates, after baseline normalization.

Orthogonal polynomials are preferred over natural polynomials because
adding a higher degree does not change the lower-order coefficients.
This makes them more stable for comparative analysis.

The curve is automatically baseline-normalized using the first and last
points as landmarks, positioned at (-0.5, 0) and (0.5, 0) respectively.

### Choosing the polynomial degree

The default `degree = 5` works well for most open curves. Higher degrees
capture more detail but increase dimensionality. Use `raw = TRUE` to
examine the R² value and assess fit quality.

## See also

[`opoly_i()`](https://momx.github.io/Momocs2/reference/opoly_i.md) for
inverse transform,
[`npoly()`](https://momx.github.io/Momocs2/reference/npoly.md) for
natural polynomials,
[`dct()`](https://momx.github.io/Momocs2/reference/dct.md) for discrete
cosine transform.

## Examples

``` r
# Single curve
data(olea)
cur <- olea$VD[[1]]
opoly_coefs <- opoly(cur)
opoly_coefs
#>           x0           x1           x2           x3           x4           x5 
#>  0.209371010  0.019919359 -0.953192890 -0.030751382 -0.119751996  0.008630917 
#> attr(,"class")
#> [1] "opoly"   "numeric"

# Get raw output with R² and model details
opoly(cur, raw = TRUE)
#> $coeff
#>           x0           x1           x2           x3           x4           x5 
#>  0.209371010  0.019919359 -0.953192890 -0.030751382 -0.119751996  0.008630917 
#> 
#> $degree
#> [1] 5
#> 
#> $baseline1
#> [1] -0.5  0.0
#> 
#> $baseline2
#> [1] 0.5 0.0
#> 
#> $r2
#> [1] 0.998722
#> 
#> $mod
#> 
#> Call:
#> lm(formula = coo[, 2] ~ x_poly)
#> 
#> Coefficients:
#> (Intercept)      x_poly1      x_poly2      x_poly3      x_poly4      x_poly5  
#>    0.209371     0.019919    -0.953193    -0.030751    -0.119752     0.008631  
#> 
#> 

# With higher degree
opoly(cur, degree = 8)
#>           x0           x1           x2           x3           x4           x5 
#>  0.209371010  0.019919359 -0.953192890 -0.030751382 -0.119751996  0.008630917 
#>           x6           x7           x8 
#> -0.002311164  0.014570517 -0.001177771 
#> attr(,"class")
#> [1] "opoly"   "numeric"

# List of curves
cur_list <- list(cur, cur * 1.5, cur * 2)
opoly(cur_list)
#> [[1]]
#>           x0           x1           x2           x3           x4           x5 
#>  0.209371010  0.019919359 -0.953192890 -0.030751382 -0.119751996  0.008630917 
#> attr(,"class")
#> [1] "opoly"   "numeric"
#> 
#> [[2]]
#>           x0           x1           x2           x3           x4           x5 
#>  0.209371010  0.019919359 -0.953192890 -0.030751382 -0.119751996  0.008630917 
#> attr(,"class")
#> [1] "opoly"   "numeric"
#> 
#> [[3]]
#>           x0           x1           x2           x3           x4           x5 
#>  0.209371010  0.019919359 -0.953192890 -0.030751382 -0.119751996  0.008630917 
#> attr(,"class")
#> [1] "opoly"   "numeric"
#> 
#> attr(,"class")
#> [1] "opoly" "coe"   "list" 

if (FALSE) { # \dontrun{
# Tibble - processes all coo columns by default
olea %>%
  opoly(degree = 6)  # Creates VD_coe and VL_coe

# Process specific column only
olea %>%
  opoly(.cols = VD)

# Custom name for single column
olea %>%
  opoly(.cols = VL, .name = "custom_name")
} # }
```
