# Split DCT coefficient vector into components

Separate a flattened DCT coefficient vector into its An and Bn
components.

## Usage

``` r
dct_split(x)
```

## Arguments

- x:

  Numeric vector of DCT coefficients (length must be even).

## Value

List with two elements:

- `an`: An coefficients (real parts)

- `bn`: Bn coefficients (imaginary parts)

## See also

[`dct()`](https://momx.github.io/Momocs2/reference/dct.md) to generate
coefficients

## Examples

``` r
# Create coefficient vector
data(olea)
coefs <- dct(olea$VD[[1]], nb_h = 12)
dct_split(coefs)
#> $an
#>          A1          A2          A3          A4          A5          A6 
#> -3.11820730 -0.13206715 -0.24781390 -0.09660325 -0.06788311 -0.06691169 
#>          A7          A8          A9         A10         A11         A12 
#> -0.03519719 -0.06016120 -0.02071002 -0.06544994 -0.01169704 -0.06810722 
#> 
#> $bn
#>           B1           B2           B3           B4           B5           B6 
#>  0.032926049 -0.914830858  0.005334948 -0.268975696 -0.006644877 -0.101625518 
#>           B7           B8           B9          B10          B11          B12 
#>  0.003834764 -0.049467452  0.003042230 -0.028964333 -0.002260202 -0.022833346 
#> 
```
