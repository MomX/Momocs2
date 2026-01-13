# Split EFT coefficient vector into components

Separate a flattened EFT coefficient vector into its An, Bn, Cn, Dn
components.

## Usage

``` r
efourier_split(x)
```

## Arguments

- x:

  Numeric vector of EFT coefficients (length must be divisible by 4).

## Value

List with four elements:

- `an`: An coefficients (x-axis cosine terms)

- `bn`: Bn coefficients (x-axis sine terms)

- `cn`: Cn coefficients (y-axis cosine terms)

- `dn`: Dn coefficients (y-axis sine terms)

## See also

[`efourier()`](https://momx.github.io/Momocs2/reference/efourier.md) to
generate coefficients

## Examples

``` r
# Create coefficient vector
coefs <- efourier(matrix(runif(100), ncol = 2), nb_h = 6)
efourier_split(coefs)
#> $an
#>           A1           A2           A3           A4           A5           A6 
#> -0.050294219 -0.038716907  0.001655831  0.004952233  0.015147412  0.006719248 
#> 
#> $bn
#>           B1           B2           B3           B4           B5           B6 
#> -0.042307545 -0.084310905  0.014563690  0.002632489 -0.011512931  0.057282425 
#> 
#> $cn
#>          C1          C2          C3          C4          C5          C6 
#>  0.04283153 -0.04661398  0.04709510  0.03652099 -0.05934322  0.04010071 
#> 
#> $dn
#>            D1            D2            D3            D4            D5 
#>  0.0201468308 -0.0491811212  0.0744870698 -0.0356520545 -0.0009537226 
#>            D6 
#>  0.0437534526 
#> 
```
