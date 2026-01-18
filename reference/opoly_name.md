# Name orthogonal polynomial coefficients

Add standard names to an opoly coefficient vector.

## Usage

``` r
opoly_name(x)
```

## Arguments

- x:

  Numeric vector of opoly coefficients (length = degree + 1).

## Value

Named numeric vector with names in the format x0, x1, x2, ..., xN where
x0 is the intercept.

## Details

Names are assigned as x0 (intercept), x1, x2, ..., xN where N is the
polynomial degree.

## Examples

``` r
# Create unnamed coefficient vector
coefs <- runif(6)  # degree 5
opoly_name(coefs)
#>        x0        x1        x2        x3        x4        x5 
#> 0.2132481 0.7920109 0.5926344 0.6062636 0.1107685 0.1956086 
```
