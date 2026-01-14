# Translate shape to y-axis

Translate a shape so its centroid aligns with the y-axis.

## Usage

``` r
coo_translate_to_yaxis(x, ..., .cols = NULL, .ldk_col = NULL)
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

- If `x` is a single matrix: returns the transformed matrix

- If `x` is a list: returns a list of transformed matrices

- If `x` is a tibble: returns the tibble with specified coo columns
  transformed

## See also

[`coo_translate_to_xaxis()`](https://momx.github.io/Momocs2/reference/coo_translate_to_xaxis.md)
for x-axis;
[`coo_center()`](https://momx.github.io/Momocs2/reference/coo_center.md)
for centering

## Examples

``` r
# Translate to y-axis
coo_translate_to_yaxis(shapes$cat)
#> <xy [120 x 2]>
#>       [,1]   [,2]  
#>  [1,] 11.875 62.000
#>  [2,]  7.875 56.000
#>  [3,] 11.875 56.000
#>  [4,] 17.875 54.000
#>  [5,] 20.875 48.000
#>  [6,] ...    ...   
#>  [7,] 30.875 45.000
#>  [8,] 24.875 47.000
#>  [9,] 19.875 53.000
#> [10,] 15.875 59.000
#> [11,] 13.875 65.000

# Works on tibbles
coo_translate_to_yaxis(bot)
#> # A tibble: 40 × 5
#>    id           coo       type   fake  price
#>    <chr>        <out>     <fct>  <fct> <dbl>
#>  1 brahma       (138 x 2) whisky a       3  
#>  2 caney        (168 x 2) whisky a       1.2
#>  3 chimay       (189 x 2) whisky a       3.8
#>  4 corona       (129 x 2) whisky a       2.6
#>  5 deusventrue  (152 x 2) whisky a       1.1
#>  6 duvel        (161 x 2) whisky a       3.1
#>  7 franziskaner (124 x 2) whisky a       2.6
#>  8 grimbergen   (126 x 2) whisky a       2.9
#>  9 guiness      (183 x 2) whisky a       1.2
#> 10 hoegardeen   (193 x 2) whisky a       3.6
#> # ℹ 30 more rows
```
