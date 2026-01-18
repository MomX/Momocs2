# Name elliptic Fourier coefficients

Add standard names to an EFT coefficient vector.

## Usage

``` r
eft_name(x)
```

## Arguments

- x:

  Numeric vector of EFT coefficients (length must be divisible by 4).

## Value

Named numeric vector with names in the format A1, A2, ..., An, B1, B2,
..., Bn, C1, ..., D1, ...

## Details

Names are assigned in the order: An (x cosine), Bn (x sine), Cn (y
cosine), Dn (y sine), where n is the harmonic number.

## Examples

``` r
# Create unnamed coefficient vector
coefs <- runif(24)  # 6 harmonics
eft_name(coefs)
#>         A1         A2         A3         A4         A5         A6         B1 
#> 0.24258702 0.75917095 0.02910415 0.43734078 0.53040949 0.42983135 0.80929696 
#>         B2         B3         B4         B5         B6         C1         C2 
#> 0.92256462 0.67502347 0.09396819 0.19746332 0.47554513 0.71437163 0.53807928 
#>         C3         C4         C5         C6         D1         D2         D3 
#> 0.87538301 0.29358541 0.22476543 0.69091783 0.31232940 0.48329818 0.12179664 
#>         D4         D5         D6 
#> 0.69000723 0.31573140 0.81618768 
```
