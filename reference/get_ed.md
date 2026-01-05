# Euclidean distance measurements

Calculate Euclidean distances between points or shapes.

## Usage

``` r
get_ed(x, y)

get_ed_along(x, y)

get_ed_intermediate(x, y, r)
```

## Arguments

- x:

  A matrix (nx2) or numeric vector of length 2.

- y:

  A matrix (nx2) or numeric vector of length 2.

- r:

  Numeric. Interpolation parameter between 0 and 1.

## Value

- `get_ed()`: numeric scalar

- `get_ed_along()`: numeric vector

- `get_ed_intermediate()`: matrix (same dimensions as inputs)

## Details

- `get_ed()`: distance between two points or total distance between
  shapes

- `get_ed_along()`: point-wise distances between two conformable
  matrices

- `get_ed_intermediate()`: interpolated point(s) along line between x
  and y

These functions do not use the `make_get_function` dispatcher as they
require two arguments (x and y).

## Examples

``` r
p1 <- c(0, 0)
p2 <- c(3, 4)
get_ed(p1, p2)
#> [1] 5

get_ed_along(shapes$cat, shapes$dog)
#>   [1]  44.00000  49.04080  51.66237  57.69749  68.60029  74.79305  74.43118
#>   [8]  69.33974  63.24555  58.25805  54.48853  50.93133  46.40043  40.49691
#>  [15]  34.00000  21.54066  16.15549  22.84732  33.54102  46.64762  57.42822
#>  [22]  67.00746  76.69420  85.79627  92.02174  95.46203  99.76472 106.88779
#>  [29] 108.97706 115.42097 115.60277 115.00000 113.89908 111.39569 112.64546
#>  [36] 114.23660 116.73046 118.47363 122.38055 124.45481 126.69649 126.15070
#>  [43] 125.32358 126.00397 123.19903 119.82070 115.41230 110.02272 106.48005
#>  [50] 102.45975 100.72239  98.48858  96.51943  95.77578  95.46203  97.30879
#>  [57]  97.65244 100.49876 102.88343 104.65180 106.02358 109.59015 109.59015
#>  [64] 109.12378 105.54620 104.31683 103.46497 105.11898 104.84751 102.57680
#>  [71] 102.41582 105.00476 105.53672 109.03669 111.36427 111.07205 107.22873
#>  [78] 105.72133 104.04326  97.98980  87.66413  77.87811  71.17584  68.88396
#>  [85]  62.68174  58.25805  53.75872  50.80354  50.21952  53.41348  56.72742
#>  [92]  59.54830  58.89822  57.20140  55.75841  55.22681  56.08030  56.08030
#>  [99]  57.14018  60.80296  63.25346  66.60330  70.76722  73.10951  67.20863
#> [106]  59.41380  48.41487  41.14608  36.49658  33.73426  33.28663  32.80244
#> [113]  40.85340  51.92302  60.63827  61.39218  59.43904  53.15073  47.85394
#> [120]  41.19466
get_ed_intermediate(p1, p2, r = 0.5)
#> [1] 1.5 2.0
```
