# Get angles from centroid

Calculate the angle from the centroid to each point on the shape.

## Usage

``` r
get_angles(x, ..., .cols = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

## Value

- If `x` is a single matrix: returns a numeric vector of angles in
  radians

- If `x` is a list: returns a list of numeric vectors

- If `x` is a tibble: returns a list of numeric vectors extracted from
  coo column

## Details

Angles are computed using [`atan2()`](https://rdrr.io/r/base/Trig.html)
on centered coordinates. Useful for angular resampling or finding points
in specific directions.

For tibbles, this function extracts values without modifying the tibble.

## Examples

``` r
get_angles(shapes$cat)
#>   [1] -1.21748216 -1.36749260 -1.26940271 -1.15239891 -1.14641345 -1.25241869
#>   [7] -1.34289227 -1.45872007 -1.57319096 -1.68759940 -1.80332893 -1.91103364
#>  [13] -2.01113261 -2.10262708 -2.18524837 -2.26895373 -2.27425278 -2.24096317
#>  [19] -2.16229542 -2.07436041 -1.99464259 -1.91360390 -1.83565089 -1.75646541
#>  [25] -1.67842403 -1.59942731 -1.51790853 -1.43486689 -1.36761433 -1.31240925
#>  [31] -1.39736690 -1.47411593 -1.54826408 -1.61974769 -1.68907962 -1.75784450
#>  [37] -1.82486753 -1.89317027 -1.96002731 -2.01980660 -2.09416690 -2.18046712
#>  [43] -2.25780081 -2.33043024 -2.38199948 -2.43916222 -2.48559573 -2.53070879
#>  [49] -2.60223180 -2.69126986 -2.77824005 -2.88501936 -2.99123491 -3.10170452
#>  [55]  3.07276729  2.96565303  2.85756798  2.75869946  2.65163533  2.53893823
#>  [61]  2.42967267  2.34860397  2.23932504  2.12463396  2.00504540  1.90875678
#>  [67]  1.81820280  1.72665468  1.63830572  1.57232444  1.53806315  1.53982563
#>  [73]  1.53159577  1.51479578  1.49086770  1.44304170  1.39827968  1.38324154
#>  [79]  1.36237301  1.33495416  1.29799167  1.27835530  1.26696903  1.24180741
#>  [85]  1.18760361  1.13690239  1.08097521  1.02750045  1.00747014  1.00916603
#>  [91]  1.03393803  1.03416451  0.99873022  0.95597415  0.90165493  0.85145905
#>  [97]  0.79477210  0.73069154  0.67863122  0.63816208  0.58592206  0.50431333
#> [103]  0.38685227  0.23295483  0.08496605 -0.09703272 -0.27286766 -0.43308270
#> [109] -0.57275826 -0.67653837 -0.73146594 -0.68485109 -0.72918300 -0.82356509
#> [115] -0.94076242 -1.01037211 -1.08577281 -1.12132523 -1.14711583 -1.12720829

# Extract from tibble
angles <- get_angles(bot)

# Add to tibble
bot$angles <- get_angles(bot)
```
