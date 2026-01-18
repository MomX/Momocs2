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
#> 0.10733189 0.07299444 0.35631458 0.57156084 0.67798806 0.69071910 0.37341082 
#>         B2         B3         B4         B5         B6         C1         C2 
#> 0.09797805 0.09900971 0.77480158 0.15149556 0.39460658 0.93499840 0.73958956 
#>         C3         C4         C5         C6         D1         D2         D3 
#> 0.63566812 0.33821985 0.96241630 0.11178991 0.65048527 0.98351155 0.17912750 
#>         D4         D5         D6 
#> 0.13797044 0.05655859 0.54636762 
```
