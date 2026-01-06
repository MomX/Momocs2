# Filter points by direction from centroid

Keep only points in a specific direction relative to the centroid.

## Usage

``` r
coo_up(x, ..., .cols = NULL, .ldk_col = NULL)

coo_down(x, ..., .cols = NULL, .ldk_col = NULL)

coo_right(x, ..., .cols = NULL, .ldk_col = NULL)

coo_left(x, ..., .cols = NULL, .ldk_col = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo columns.

- .ldk_col:

  Character. Name of landmark column. If `NULL`, uses `colname_ldk`.

## Value

- If `x` is a single matrix: returns the filtered matrix (open curve)

- If `x` is a list: returns a list of filtered matrices

- If `x` is a tibble: returns the tibble with filtered coo column(s)

## Details

Filters points based on their position relative to the centroid:

- **`coo_up()`**: Keep points with y \> centroid_y

- **`coo_down()`**: Keep points with y \< centroid_y

- **`coo_right()`**: Keep points with x \> centroid_x

- **`coo_left()`**: Keep points with x \< centroid_x

Returns an open curve. Landmarks are synced.

## Examples

``` r
coo_up(shapes$cat)
#>       [,1] [,2]
#>  [1,]  133   98
#>  [2,]  133  104
#>  [3,]  134  110
#>  [4,]  134  116
#>  [5,]  136  122
#>  [6,]  139  128
#>  [7,]  142  134
#>  [8,]  144  139
#>  [9,]  148  145
#> [10,]  153  151
#> [11,]  159  157
#> [12,]  165  160
#> [13,]  171  162
#> [14,]  177  165
#> [15,]  183  170
#> [16,]  188  176
#> [17,]  191  182
#> [18,]  191  187
#> [19,]  192  193
#> [20,]  194  199
#> [21,]  197  205
#> [22,]  203  210
#> [23,]  209  214
#> [24,]  212  220
#> [25,]  216  226
#> [26,]  221  231
#> [27,]  225  226
#> [28,]  226  220
#> [29,]  226  215
#> [30,]  228  211
#> [31,]  234  208
#> [32,]  239  204
#> [33,]  244  199
#> [34,]  249  195
#> [35,]  248  189
#> [36,]  244  183
#> [37,]  238  178
#> [38,]  235  173
#> [39,]  235  167
#> [40,]  236  162
#> [41,]  237  156
#> [42,]  237  150
#> [43,]  237  144
#> [44,]  237  138
#> [45,]  235  132
#> [46,]  231  126
#> [47,]  227  120
#> [48,]  224  114
#> [49,]  222  108
#> [50,]  221  102
#> [51,]  221   97
#> attr(,"class")
#> [1] "coo"    "matrix" "array" 
coo_right(shapes$cat)
#>       [,1] [,2]
#>  [1,]  200   62
#>  [2,]  196   56
#>  [3,]  200   56
#>  [4,]  206   54
#>  [5,]  209   48
#>  [6,]  205   43
#>  [7,]  200   43
#>  [8,]  194   42
#>  [9,]  192   21
#> [10,]  198   22
#> [11,]  203   22
#> [12,]  208   19
#> [13,]  202   15
#> [14,]  196   13
#> [15,]  190   11
#> [16,]  191  182
#> [17,]  191  187
#> [18,]  192  193
#> [19,]  194  199
#> [20,]  197  205
#> [21,]  203  210
#> [22,]  209  214
#> [23,]  212  220
#> [24,]  216  226
#> [25,]  221  231
#> [26,]  225  226
#> [27,]  226  220
#> [28,]  226  215
#> [29,]  228  211
#> [30,]  234  208
#> [31,]  239  204
#> [32,]  244  199
#> [33,]  249  195
#> [34,]  248  189
#> [35,]  244  183
#> [36,]  238  178
#> [37,]  235  173
#> [38,]  235  167
#> [39,]  236  162
#> [40,]  237  156
#> [41,]  237  150
#> [42,]  237  144
#> [43,]  237  138
#> [44,]  235  132
#> [45,]  231  126
#> [46,]  227  120
#> [47,]  224  114
#> [48,]  222  108
#> [49,]  221  102
#> [50,]  221   97
#> [51,]  221   91
#> [52,]  221   85
#> [53,]  221   79
#> [54,]  221   73
#> [55,]  222   67
#> [56,]  224   62
#> [57,]  230   60
#> [58,]  232   55
#> [59,]  230   49
#> [60,]  224   45
#> [61,]  219   45
#> [62,]  213   47
#> [63,]  208   53
#> [64,]  204   59
#> [65,]  202   65
#> attr(,"class")
#> [1] "coo"    "matrix" "array" 
```
