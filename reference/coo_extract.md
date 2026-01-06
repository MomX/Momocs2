# Extract or subset coordinates

Subset or trim shapes by row index or position.

## Usage

``` r
coo_extract(x, ..., .cols = NULL, .ldk_col = NULL)

coo_head(x, ..., .cols = NULL, .ldk_col = NULL)

coo_tail(x, ..., .cols = NULL, .ldk_col = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

- .ldk_col:

  Character. Name of landmark column. If `NULL`, uses `colname_ldk`.

- id:

  Integer vector of row indices to extract (for `coo_extract()`).

- n:

  Integer. Number of points to keep or remove.

## Value

- If `x` is a single matrix: returns a matrix with selected/trimmed rows

- If `x` is a list: returns a list of matrices

- If `x` is a tibble: returns the tibble with specified coo columns
  subsetted

## Details

- `coo_extract()`: extract specific rows by index

- `coo_head()`: keep first n points

- `coo_tail()`: keep all but the last n points

If `n` equals `nrow(x)`, the shape is returned unchanged.

These functions are landmark-aware: invalid landmark indices are
automatically filtered out after subsetting.

## See also

[`coo_sample()`](https://momx.github.io/Momocs2/reference/coo_sample.md)
for resampling to different point counts

## Examples

``` r
coo_extract(shapes$cat, c(1, 10, 50))
#>      [,1] [,2]
#> [1,]  200   62
#> [2,]  182   42
#> [3,]  136   69
#> attr(,"class")
#> [1] "coo"    "matrix" "array" 
coo_extract(shapes, c(1, 6))
#> $cat
#>      [,1] [,2]
#> [1,]  200   62
#> [2,]  205   43
#> 
#> $dog
#>      [,1] [,2]
#> [1,]  200  106
#> [2,]  168  108
#> 
#> $heart
#>      [,1] [,2]
#> [1,]  200   37
#> [2,]  185   56
#> 
#> $leaf2
#>      [,1] [,2]
#> [1,]  200   62
#> [2,]  194   29
#> 
#> attr(,"class")
#> [1] "coo"  "list"
coo_head(shapes$cat, 10)
#>       [,1] [,2]
#>  [1,]  200   62
#>  [2,]  196   56
#>  [3,]  200   56
#>  [4,]  206   54
#>  [5,]  209   48
#>  [6,]  205   43
#>  [7,]  200   43
#>  [8,]  194   42
#>  [9,]  188   42
#> [10,]  182   42
#> attr(,"class")
#> [1] "coo"    "matrix" "array" 
coo_head(shapes, 3)
#> $cat
#>      [,1] [,2]
#> [1,]  200   62
#> [2,]  196   56
#> [3,]  200   56
#> 
#> $dog
#>      [,1] [,2]
#> [1,]  200  106
#> [2,]  194  105
#> [3,]  187  106
#> 
#> $heart
#>      [,1] [,2]
#> [1,]  200   37
#> [2,]  198   39
#> [3,]  197   42
#> 
#> $leaf2
#>      [,1] [,2]
#> [1,]  200   62
#> [2,]  200   55
#> [3,]  199   49
#> 
#> attr(,"class")
#> [1] "coo"  "list"
coo_tail(shapes$cat, 5)
#>        [,1] [,2]
#>   [1,]  200   62
#>   [2,]  196   56
#>   [3,]  200   56
#>   [4,]  206   54
#>   [5,]  209   48
#>   [6,]  205   43
#>   [7,]  200   43
#>   [8,]  194   42
#>   [9,]  188   42
#>  [10,]  182   42
#>  [11,]  176   43
#>  [12,]  170   43
#>  [13,]  164   43
#>  [14,]  158   43
#>  [15,]  152   43
#>  [16,]  146   44
#>  [17,]  143   41
#>  [18,]  142   36
#>  [19,]  145   30
#>  [20,]  150   25
#>  [21,]  156   23
#>  [22,]  162   21
#>  [23,]  168   20
#>  [24,]  174   19
#>  [25,]  180   19
#>  [26,]  186   20
#>  [27,]  192   21
#>  [28,]  198   22
#>  [29,]  203   22
#>  [30,]  208   19
#>  [31,]  202   15
#>  [32,]  196   13
#>  [33,]  190   11
#>  [34,]  184   10
#>  [35,]  178    9
#>  [36,]  172    9
#>  [37,]  166    9
#>  [38,]  160   10
#>  [39,]  154   11
#>  [40,]  149   13
#>  [41,]  143   16
#>  [42,]  137   21
#>  [43,]  133   27
#>  [44,]  130   33
#>  [45,]  130   39
#>  [46,]  130   45
#>  [47,]  132   51
#>  [48,]  135   57
#>  [49,]  136   63
#>  [50,]  136   69
#>  [51,]  135   74
#>  [52,]  134   80
#>  [53,]  134   86
#>  [54,]  133   92
#>  [55,]  133   98
#>  [56,]  133  104
#>  [57,]  134  110
#>  [58,]  134  116
#>  [59,]  136  122
#>  [60,]  139  128
#>  [61,]  142  134
#>  [62,]  144  139
#>  [63,]  148  145
#>  [64,]  153  151
#>  [65,]  159  157
#>  [66,]  165  160
#>  [67,]  171  162
#>  [68,]  177  165
#>  [69,]  183  170
#>  [70,]  188  176
#>  [71,]  191  182
#>  [72,]  191  187
#>  [73,]  192  193
#>  [74,]  194  199
#>  [75,]  197  205
#>  [76,]  203  210
#>  [77,]  209  214
#>  [78,]  212  220
#>  [79,]  216  226
#>  [80,]  221  231
#>  [81,]  225  226
#>  [82,]  226  220
#>  [83,]  226  215
#>  [84,]  228  211
#>  [85,]  234  208
#>  [86,]  239  204
#>  [87,]  244  199
#>  [88,]  249  195
#>  [89,]  248  189
#>  [90,]  244  183
#>  [91,]  238  178
#>  [92,]  235  173
#>  [93,]  235  167
#>  [94,]  236  162
#>  [95,]  237  156
#>  [96,]  237  150
#>  [97,]  237  144
#>  [98,]  237  138
#>  [99,]  235  132
#> [100,]  231  126
#> [101,]  227  120
#> [102,]  224  114
#> [103,]  222  108
#> [104,]  221  102
#> [105,]  221   97
#> [106,]  221   91
#> [107,]  221   85
#> [108,]  221   79
#> [109,]  221   73
#> [110,]  222   67
#> [111,]  224   62
#> [112,]  230   60
#> [113,]  232   55
#> [114,]  230   49
#> [115,]  224   45
#> attr(,"class")
#> [1] "coo"    "matrix" "array" 
coo_head(bot, 20)
#> # A tibble: 40 × 3
#>    coo    type   dummy
#>    <out>  <fct>  <fct>
#>  1 (20·2) whisky a    
#>  2 (20·2) whisky a    
#>  3 (20·2) whisky a    
#>  4 (20·2) whisky a    
#>  5 (20·2) whisky a    
#>  6 (20·2) whisky a    
#>  7 (20·2) whisky a    
#>  8 (20·2) whisky a    
#>  9 (20·2) whisky a    
#> 10 (20·2) whisky a    
#> # ℹ 30 more rows
```
