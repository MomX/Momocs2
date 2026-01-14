# Wrap coordinates from rows to list-column of matrices

Converts x and y coordinate columns into a list-column of matrices, with
one matrix per group (defined by other columns or an `.id` column).

## Usage

``` r
wrap(.data, x = x, y = y, .into = "coo", .class = "coo")
```

## Arguments

- .data:

  A tibble with x and y coordinate columns

- x:

  Column name for x coordinates (unquoted). Default is `x`.

- y:

  Column name for y coordinates (unquoted). Default is `y`.

- .into:

  Name of the new list-column to create. Default is `"coo"`.

- .class:

  Character vector of classes to PREFIX to the list-column. The
  list-column always has base classes `c("coo", "list")`. If `.class` is
  provided, these classes are prepended, e.g., `.class = "out"` results
  in `c("out", "coo", "list")`. Individual matrices always get class
  `c("xy", "matrix", "array")` regardless of this parameter. Set to
  `NULL` for just `c("coo", "list")`.

## Value

A tibble with x and y columns removed and replaced by a list-column
containing coordinate matrices (one per group). Individual matrices have
class `c("xy", "matrix", "array")` with column names `c("x", "y")`. The
list-column has class `c(.class, "coo", "list")` if `.class` is
provided, or `c("coo", "list")` if `.class = NULL`.

## Details

The function groups rows by all columns except x and y, then wraps each
group's coordinates into a matrix. This is the inverse of
[`unwrap()`](https://momx.github.io/Momocs2/reference/unwrap.md).

Grouping behavior:

- If `.data` has grouping (from
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)),
  uses existing groups

- If no groups, wraps ALL rows into a single matrix (assumes single
  shape)

- For multiple shapes, ensure you have identifying columns or use `.id`
  in [`unwrap()`](https://momx.github.io/Momocs2/reference/unwrap.md)

## See also

[`unwrap()`](https://momx.github.io/Momocs2/reference/unwrap.md) for the
reverse operation

## Examples

``` r
# Start with unwrapped coordinates
df_unwrapped <- tibble::tibble(
  id = rep(1:3, each = 4),
  x = c(0, 1, 1, 0,  2, 3, 3, 2,  4, 5, 5, 4),
  y = c(0, 0, 1, 1,  0, 0, 1, 1,  0, 0, 1, 1)
)

# Wrap by id (x and y are defaults)
df_wrapped <- df_unwrapped %>% wrap()
df_wrapped
#> # A tibble: 3 × 2
#>      id coo    
#>   <int> <coo>  
#> 1     1 (4 x 2)
#> 2     2 (4 x 2)
#> 3     3 (4 x 2)
# A tibble: 3 × 2
#      id coo
#   <int> <coo>
# 1     1 (4 x 2)
# 2     2 (4 x 2)
# 3     3 (4 x 2)

# Explicit column names
df_wrapped <- df_unwrapped %>% wrap(x, y, .into = "coo")

# Individual matrices have "xy" class
df_wrapped$coo[[1]]
#> <xy [4 x 2]>
#>      x y
#> [1,] 0 0
#> [2,] 1 0
#> [3,] 1 1
#> [4,] 0 1
class(df_wrapped$coo[[1]])
#> [1] "xy"     "matrix" "array" 
# [1] "xy"     "matrix" "array"

# List-column has "coo" class
class(df_wrapped$coo)
#> [1] "coo"  "coo"  "list"
# [1] "coo"  "list"

# Prefix with "out" for outlines
df_out <- wrap(df_unwrapped, .into = "outline", .class = "out")
class(df_out$outline)
#> [1] "out"  "coo"  "list"
# [1] "out"  "coo"  "list"

# No prefix classes (just base "coo")
wrap(df_unwrapped, .class = NULL)
#> # A tibble: 3 × 2
#>      id coo    
#>   <int> <coo>  
#> 1     1 (4 x 2)
#> 2     2 (4 x 2)
#> 3     3 (4 x 2)
```
