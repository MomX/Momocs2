# Name natural polynomial coefficients

Add standard names to an npoly coefficient vector.

## Usage

``` r
npoly_name(x)
```

## Arguments

- x:

  Numeric vector of npoly coefficients (length = degree + 1).

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
npoly_name(coefs)
#>           x0           x1           x2           x3           x4           x5 
#> 0.6619992773 0.0009579435 0.9759655297 0.2201634564 0.3063278385 0.7315666373 
```
