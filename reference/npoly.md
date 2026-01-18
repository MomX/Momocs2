# Natural Polynomial Transform

Compute natural polynomial coefficients from open curve coordinates.

## Usage

``` r
npoly(x, degree = 5, raw = FALSE, ..., .cols = NULL, .name = "_coe")
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
  vector with classes `c("npoly", "numeric")` containing degree+1
  coefficients (x0, x1, ..., xN)

- If `x` is a single matrix and `raw = TRUE`: returns a list with
  elements `coeff`, `degree`, `baseline1`, `baseline2`, `r2`, `mod`

- If `x` is a list: returns a list of coefficient vectors (each with
  class `c("npoly", "numeric")`)

- If `x` is a tibble: returns the tibble with new list-column(s) of
  class `c("npoly", "coe", "list")` containing coefficient vectors

## Details

Natural polynomial fitting uses standard power basis polynomials (1, x,
x², x³, ...) to decompose open curves. The method fits a polynomial
model to the y-coordinates as a function of x-coordinates, after
baseline normalization.

Unlike orthogonal polynomials, natural polynomials have coefficients
that change when higher degrees are added. However, they are more
interpretable and directly correspond to the polynomial equation.

The curve is automatically baseline-normalized using the first and last
points as landmarks, positioned at (-0.5, 0) and (0.5, 0) respectively.

### Choosing the polynomial degree

The default `degree = 5` works well for most open curves. Higher degrees
capture more detail but increase dimensionality and risk overfitting.
Use `raw = TRUE` to examine the R² value and assess fit quality.

## See also

[`npoly_i()`](https://momx.github.io/Momocs2/reference/npoly_i.md) for
inverse transform,
[`opoly()`](https://momx.github.io/Momocs2/reference/opoly.md) for
orthogonal polynomials,
[`dct()`](https://momx.github.io/Momocs2/reference/dct.md) for discrete
cosine transform.

## Examples

``` r
# Single curve
data(olea)
cur <- olea$VD[[1]]
npoly_coefs <- npoly(cur)
npoly_coefs
#>          x0          x1          x2          x3          x4          x5 
#>  0.31027929  0.03654682 -0.64016581 -0.35437960 -2.26592353  0.64569559 
#> attr(,"class")
#> [1] "npoly"   "numeric"

# Get raw output with R² and model details
npoly(cur, raw = TRUE)
#> $coeff
#>          x0          x1          x2          x3          x4          x5 
#>  0.31027929  0.03654682 -0.64016581 -0.35437960 -2.26592353  0.64569559 
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
#>     0.31028      0.03655     -0.64017     -0.35438     -2.26592      0.64570  
#> 
#> 

# With higher degree
npoly(cur, degree = 8)
#>          x0          x1          x2          x3          x4          x5 
#>  0.31045114  0.01056954 -0.64587938  0.53074384 -2.39943119 -6.75387457 
#>          x6          x7          x8 
#>  1.99846549 17.47625125 -5.68546437 
#> attr(,"class")
#> [1] "npoly"   "numeric"

# List of curves
cur_list <- list(cur, cur * 1.5, cur * 2)
npoly(cur_list)
#> [[1]]
#>          x0          x1          x2          x3          x4          x5 
#>  0.31027929  0.03654682 -0.64016581 -0.35437960 -2.26592353  0.64569559 
#> attr(,"class")
#> [1] "npoly"   "numeric"
#> 
#> [[2]]
#>          x0          x1          x2          x3          x4          x5 
#>  0.31027929  0.03654682 -0.64016581 -0.35437960 -2.26592353  0.64569559 
#> attr(,"class")
#> [1] "npoly"   "numeric"
#> 
#> [[3]]
#>          x0          x1          x2          x3          x4          x5 
#>  0.31027929  0.03654682 -0.64016581 -0.35437960 -2.26592353  0.64569559 
#> attr(,"class")
#> [1] "npoly"   "numeric"
#> 
#> attr(,"class")
#> [1] "npoly" "coe"   "list" 

if (FALSE) { # \dontrun{
# Tibble - processes all coo columns by default
library(dplyr)
olea %>%
  npoly(degree = 6)  # Creates VD_coe and VL_coe

# Process specific column only
olea %>%
  npoly(.cols = VD)

# Custom name for single column
olea %>%
  npoly(.cols = VL, .name = "custom_name")
} # }
```
