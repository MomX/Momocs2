# Cut outline at landmarks

Split an outline into open curves at landmark positions.

## Usage

``` r
get_cut(x, ldk = NULL, ..., .cols = NULL, .ldk_col = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ldk:

  Integer vector. Landmark indices to cut at. Required for single
  matrix.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo columns.

- .ldk_col:

  Character. Name of landmark column. If `NULL`, uses `colname_ldk`.

## Value

- If `x` is a single matrix: returns a list of matrices (open curves
  with class 'cur')

- If `x` is a list: returns a list of lists (one per shape)

- If `x` is a tibble: returns a tibble with \_cur1, \_cur2, etc. columns

## Details

Splits an outline into segments between consecutive landmarks. Each
segment is an open curve that starts and ends at landmarks.

For an outline with landmarks at positions 3 and 7 (8 points total):

- Curve 1: points 3→4→5→6→7

- Curve 2: points 7→8→1→2→3 (wraps around)

Requires at least 2 landmarks. Landmarks are included at the start and
end of each curve.

## See also

[`get_join()`](https://momx.github.io/Momocs2/reference/get_join.md)

## Examples

``` r
# Cut outline at landmarks
coo <- matrix(1:20, ncol=2)
get_cut(coo, ldk = c(3, 7))
#> [[1]]
#>      [,1] [,2]
#> [1,]    3   13
#> [2,]    4   14
#> [3,]    5   15
#> [4,]    6   16
#> [5,]    7   17
#> attr(,"class")
#> [1] "cur"    "matrix"
#> 
#> [[2]]
#>      [,1] [,2]
#> [1,]    7   17
#> [2,]    8   18
#> [3,]    9   19
#> [4,]   10   20
#> [5,]    1   11
#> [6,]    2   12
#> [7,]    3   13
#> attr(,"class")
#> [1] "cur"    "matrix"
#> 
```
