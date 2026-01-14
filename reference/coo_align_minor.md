# Align shape to minor and major axes (swapped)

Align a shape to its principal axes with major and minor axes swapped.

## Usage

``` r
coo_align_minor(x, ..., .cols = NULL, .ldk_col = NULL)
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

- If `x` is a single matrix: returns the aligned matrix

- If `x` is a list: returns a list of aligned matrices

- If `x` is a tibble: returns the tibble with specified coo columns
  aligned

## See also

[`coo_align()`](https://momx.github.io/Momocs2/reference/coo_align.md)
for standard alignment

## Examples

``` r
# Align with swapped axes
coo_align_minor(shapes$cat)
#> <xy [120 x 2]>
#>       [,1]    [,2]   
#>  [1,] 208.506  66.587
#>  [2,] 206.337  59.710
#>  [3,] 210.179  60.825
#>  [4,] 216.498  60.577
#>  [5,] 221.052  55.651
#>  [6,] ...     ...    
#>  [7,] 231.492  55.558
#>  [8,] 225.172  55.806
#>  [9,] 218.698  60.174
#> [10,] 213.184  64.821
#> [11,] 209.590  70.026

# Works on tibbles
coo_align_minor(bot)
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
