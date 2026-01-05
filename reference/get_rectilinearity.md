# Get rectilinearity

Calculate rectilinearity - a measure of how well a shape fits a
rectangle after optimal rotation.

## Usage

``` r
get_rectilinearity(x, ..., .cols = NULL)
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

- If `x` is a single matrix: returns a numeric scalar

- If `x` is a list: returns a numeric vector

- If `x` is a tibble: returns a numeric vector extracted from coo column

## Details

Rectilinearity measures how rectangular a shape is by testing multiple
rotations and finding the orientation that best fits a rectangle.

Algorithm from Zunic & Rosin (2003). Higher values indicate more
rectangular shapes. Computationally intensive - tests 4n rotations where
n is the number of points.

## References

Zunic, J., & Rosin, P. L. (2003). Rectilinearity measurements for
polygons. IEEE Transactions on Pattern Analysis and Machine
Intelligence.

## Examples

``` r
get_rectilinearity(shapes$cat)
#> [1] 0.1343944

# Use in measure()
bot %>% measure("rectilinearity")
#> # A tibble: 40 × 4
#>    coo     type   dummy coo_rectilinearity
#>    <out>   <fct>  <fct>              <dbl>
#>  1 (138·2) whisky a                  0.430
#>  2 (168·2) whisky a                  0.459
#>  3 (189·2) whisky a                  0.400
#>  4 (129·2) whisky a                  0.602
#>  5 (152·2) whisky a                  0.388
#>  6 (161·2) whisky a                  0.415
#>  7 (124·2) whisky a                  0.567
#>  8 (126·2) whisky a                  0.452
#>  9 (183·2) whisky a                  0.482
#> 10 (193·2) whisky a                  0.542
#> # ℹ 30 more rows
```
