# Extract landmark coordinates

Extract the coordinates at landmark positions from shapes.

## Usage

``` r
get_ldk(x, ldk = NULL, ..., .cols = NULL, .ldk_col = NULL, .name = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ldk:

  Integer vector. Landmark indices (for single matrix or list).

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

- .ldk_col:

  Character. Name of landmark column. If `NULL`, uses `colname_ldk`.

- .name:

  Character. Name for the output column when `x` is a tibble. If `NULL`,
  uses "colname_ldk_coords" (e.g., "coo_ldk_coords").

## Value

- If `x` is a single matrix: returns a matrix with landmark coordinates

- If `x` is a list: returns a list of matrices with landmark coordinates

- If `x` is a tibble: returns the tibble with new landmark coordinates
  column added

## Details

For tibbles, this function automatically finds the landmark column using
the naming convention `colname_ldk` (e.g., `coo_ldk` for a `coo`
column).

Unlike other `get_*` functions, `get_ldk()` DOES add a column to tibbles
for convenience, since landmark coordinates are commonly needed
alongside the original coordinates.

## Examples

``` r
# Single matrix with landmark indices
get_ldk(shapes$cat, ldk = c(1, 10, 50, 100))
#>      [,1] [,2]
#> [1,]  200   62
#> [2,]  182   42
#> [3,]  136   69
#> [4,]  231  126

# List with single landmark vector (applied to all)
get_ldk(shapes, ldk = c(1, 25, 50))
#> $cat
#>      [,1] [,2]
#> [1,]  200   62
#> [2,]  180   19
#> [3,]  136   69
#> 
#> $dog
#>      [,1] [,2]
#> [1,]  200  106
#> [2,]  112   81
#> [3,]  169  166
#> 
#> $heart
#>      [,1] [,2]
#> [1,]  200   37
#> [2,]  103  127
#> [3,]  160  212
#> 
#> $leaf2
#>      [,1] [,2]
#> [1,]  200   62
#> [2,]  133   72
#> [3,]  152  163
#> 

# Tibble - auto-detect landmark column and add coordinates
bot %>% get_ldk()
#> Warning: Landmark column 'coo_ldk' not found. Skipping.
#> # A tibble: 40 × 3
#>    coo     type   dummy
#>    <out>   <fct>  <fct>
#>  1 (138·2) whisky a    
#>  2 (168·2) whisky a    
#>  3 (189·2) whisky a    
#>  4 (129·2) whisky a    
#>  5 (152·2) whisky a    
#>  6 (161·2) whisky a    
#>  7 (124·2) whisky a    
#>  8 (126·2) whisky a    
#>  9 (183·2) whisky a    
#> 10 (193·2) whisky a    
#> # ℹ 30 more rows

# Tibble with custom output name
bot %>% get_ldk(.name = "landmarks")
#> Warning: Landmark column 'coo_ldk' not found. Skipping.
#> # A tibble: 40 × 3
#>    coo     type   dummy
#>    <out>   <fct>  <fct>
#>  1 (138·2) whisky a    
#>  2 (168·2) whisky a    
#>  3 (189·2) whisky a    
#>  4 (129·2) whisky a    
#>  5 (152·2) whisky a    
#>  6 (161·2) whisky a    
#>  7 (124·2) whisky a    
#>  8 (126·2) whisky a    
#>  9 (183·2) whisky a    
#> 10 (193·2) whisky a    
#> # ℹ 30 more rows
```
