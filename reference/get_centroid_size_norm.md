# Get normalized centroid size

Calculate the normalized centroid size (centroid size divided by
perimeter).

## Usage

``` r
get_centroid_size_norm(x, ..., .cols = NULL)

centsize_norm(x, ..., .cols = NULL)
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

For tibbles, this function extracts values without modifying the tibble.
Use within `mutate()` to add as a column:
`mutate(df, cs_norm = get_centroid_size_norm(coo))`

## Examples

``` r
get_centroid_size_norm(shapes$cat)
#> [1] 77.4076
get_centroid_size_norm(shapes)
#> $cat
#> [1] 77.4076
#> 
#> $dog
#> [1] 86.78916
#> 
#> $heart
#> [1] 89.2805
#> 
#> $leaf2
#> [1] 74.38899
#> 

# Extract from tibble
sizes_norm <- get_centroid_size_norm(bot)

if (FALSE) { # \dontrun{
 # Add to tibble when dplyr is loaded
bot %>% mutate(cs_norm = get_centroid_size_norm(coo))
} # }
```
