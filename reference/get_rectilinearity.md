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

# Use in measure() on mini bot for the sake of speed
bot[1:2, ] %>% measure("rectilinearity")
#> # A tibble: 2 × 4
#>   coo       type   dummy coo_rectilinearity
#>   <out>     <fct>  <fct>              <dbl>
#> 1 (138 x 2) whisky a                  0.430
#> 2 (168 x 2) whisky a                  0.459
```
