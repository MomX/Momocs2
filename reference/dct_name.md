# Name discrete cosine transform coefficients

Add standard names to a DCT coefficient vector.

## Usage

``` r
dct_name(x)
```

## Arguments

- x:

  Numeric vector of DCT coefficients (length must be even: 2\*nb_h).

## Value

Named numeric vector with names in the format A1, A2, ..., An, B1, B2,
..., Bn where n is the number of harmonics.

## Details

Names are assigned in the order: A1-An (real parts), B1-Bn (imaginary
parts), where n is the number of harmonics.

## Examples

``` r
# Create unnamed coefficient vector
coefs <- runif(24)  # 12 harmonics
dct_name(coefs)
#>          A1          A2          A3          A4          A5          A6 
#> 0.913158011 0.204329063 0.471755611 0.385183100 0.392751929 0.218338430 
#>          A7          A8          A9         A10         A11         A12 
#> 0.007208941 0.363712143 0.526106730 0.844163393 0.135991171 0.376467331 
#>          B1          B2          B3          B4          B5          B6 
#> 0.263308648 0.497852970 0.019525251 0.087183810 0.995962664 0.942132205 
#>          B7          B8          B9         B10         B11         B12 
#> 0.124373451 0.115189302 0.366688104 0.471040036 0.784172446 0.490392813 
```
