# Split EFT coefficient vector into components

Separate a flattened EFT coefficient vector into its An, Bn, Cn, Dn
components.

## Usage

``` r
eft_split(x)
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

[`eft()`](https://momx.github.io/Momocs2/reference/eft.md) to generate
coefficients

## Examples

``` r
# Create coefficient vector
coefs <- eft(matrix(runif(100), ncol = 2), nb_h = 6)
eft_split(coefs)
#> $an
#>          A1          A2          A3          A4          A5          A6 
#> -0.04907422 -0.03344316 -0.08030020  0.04523278  0.03034634  0.01528850 
#> 
#> $bn
#>            B1            B2            B3            B4            B5 
#> -0.0001434753  0.0839245814  0.0811836958  0.0133428444  0.0680679044 
#>            B6 
#> -0.0307392533 
#> 
#> $cn
#>          C1          C2          C3          C4          C5          C6 
#>  0.08183210 -0.03644471  0.09139745 -0.04284612 -0.12401886 -0.04115889 
#> 
#> $dn
#>           D1           D2           D3           D4           D5           D6 
#>  0.076890084 -0.019115681  0.052280217 -0.074469456  0.009884818  0.057130071 
#> 
```
