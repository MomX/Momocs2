# Get centroid size

Calculate the centroid size (CS): the square root of the sum of squared
distances from each point to the centroid.

## Usage

``` r
get_centroid_size(x, ..., .cols = NULL)

centsize(x, ..., .cols = NULL)
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

Centroid size is a common size measure in geometric morphometrics. It is
scale-independent and used for allometric correction.

For tibbles, this function extracts values without modifying the tibble.
Use within `mutate()` to add as a column:
`mutate(df, cs = get_centroid_size(coo))`

## Examples

``` r
get_centroid_size(shapes$cat)
#> [1] 847.9577
get_centroid_size(shapes)
#> $cat
#> [1] 847.9577
#> 
#> $dog
#> [1] 950.7276
#> 
#> $heart
#> [1] 978.0189
#> 
#> $leaf2
#> [1] 814.8906
#> 

# Extract from tibble
sizes <- get_centroid_size(bot)

if (FALSE) { # \dontrun{
 # Add to tibble when dplyr is loaded
bot %>% mutate(centsize = get_centroid_size(coo))
} # }
```
