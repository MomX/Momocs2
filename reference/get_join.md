# Join curves into closed outline

Join multiple open curves (positionally) into a single closed outline
with endpoints as landmarks.

## Usage

``` r
get_join(...)
```

## Arguments

- ...:

  Curve matrices to join, or a single list of curves.

## Value

A list with two elements:

- `coo`: The joined outline matrix (closed)

- `ldk`: Integer vector of landmark indices (join points)

## Details

Joins curves positionally: the end of curve 1 becomes the start of curve
2, etc. Join points (shared endpoints) become landmarks and are not
duplicated.

The last point of the final curve connects back to the first point of
the first curve to close the outline.

## See also

[`get_cut()`](https://momx.github.io/Momocs2/reference/get_cut.md)

## Examples

``` r
# Create two curves
cur1 <- matrix(c(0,0, 1,0, 2,1), ncol=2, byrow=TRUE)
cur2 <- matrix(c(2,1, 2,2, 0,2, 0,0), ncol=2, byrow=TRUE)

# Join them
result <- get_join(cur1, cur2)
result$coo  # Closed outline
#>      [,1] [,2]
#> [1,]    0    0
#> [2,]    1    0
#> [3,]    2    1
#> [4,]    2    2
#> [5,]    0    2
#> [6,]    0    0
result$ldk  # Landmark at join point
#> [1] 3
```
