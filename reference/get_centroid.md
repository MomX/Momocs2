# Get centroid of a shape

Calculate the centroid (center of mass) of a shape.

## Usage

``` r
get_centroid(x, ..., .cols = NULL)

centroid(x, ..., .cols = NULL)
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

- If `x` is a single matrix: returns a numeric vector of length 2 (x, y
  coordinates)

- If `x` is a list: returns a list of numeric vectors

- If `x` is a tibble: returns a list of numeric vectors extracted from
  coo column

## Details

For tibbles, this function extracts centroid values without modifying
the tibble. Use within `mutate()` to add as a column:
`mutate(df, cent = get_centroid(coo))`

## See also

[`coo_center()`](https://momx.github.io/Momocs2/reference/coo_center.md)
for centering shapes

## Examples

``` r
get_centroid(shapes$cat)
#> [1] 188.125  94.200
get_centroid(shapes)
#> $cat
#> [1] 188.125  94.200
#> 
#> $dog
#> [1] 206.9750 127.0917
#> 
#> $heart
#> [1] 201.6417 142.3500
#> 
#> $leaf2
#> [1] 195.425 125.650
#> 

# Extract from tibble
centroids <- get_centroid(bot)

# Add to tibble
bot$centroid <- get_centroid(bot)
```
