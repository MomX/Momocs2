# Unfold a list-column into multiple columns

Expands a list-column of numeric vectors into separate columns, one for
each element of the vectors. This is the inverse operation of
[`fold()`](https://momx.github.io/Momocs2/reference/fold.md). Classes
from the list-column are copied to each resulting column. If no column
is specified, automatically detects the coe column.

## Usage

``` r
unfold(.data, col, .prefix = NULL)
```

## Arguments

- .data:

  A data frame or tibble

- col:

  Name of the list-column to unfold (unquoted). If missing,
  automatically detects the single coe column using
  [`get_coe_cols()`](https://momx.github.io/Momocs2/reference/get_coe_cols.md).

- .prefix:

  Character string to prefix column names with. Default is to use the
  original column name followed by underscore (e.g., "coe\_"). Set to
  `""` to use original names without prefix, or provide a custom prefix.

## Value

A tibble with the list-column removed and replaced by multiple numeric
columns (one per vector element). Column names are prefixed by default.
Each column inherits classes from the original list-column (e.g., if the
list-column had class `c("eft", "coe")`, each resulting column will
too).

## Details

The function:

1.  Auto-detects the coe column if not specified

2.  Extracts the list-column and its classes

3.  Stacks all vectors into a matrix (by rows)

4.  Converts the matrix to a tibble with prefixed column names

5.  Copies classes from the list-column to each new column

6.  Removes the original list-column and binds the new columns

This is useful when you need to access individual coefficient values for
statistical analysis, plotting, or further manipulation.

## See also

[`fold()`](https://momx.github.io/Momocs2/reference/fold.md) for the
reverse operation,
[`get_coe_cols()`](https://momx.github.io/Momocs2/reference/get_coe_cols.md)
for detection

## Examples

``` r
# Start with folded data
df_folded <- tibble::tibble(
  id = 1:3,
  coe = list(
    c(A1 = 1, A2 = 4, B1 = 7, B2 = 10),
    c(A1 = 2, A2 = 5, B1 = 8, B2 = 11),
    c(A1 = 3, A2 = 6, B1 = 9, B2 = 12)
  )
)
class(df_folded$coe) <- c("eft", "coe", "list")

# Auto-detect coe column
df_unfolded <- unfold(df_folded)
# Or explicit
df_unfolded <- unfold(df_folded, coe)

# Unfold with default prefix (column name + "_")
df_unfolded
#> # A tibble: 3 × 5
#>      id coe_A1      coe_A2      coe_B1      coe_B2     
#>   <int> <eft>       <eft>       <eft>       <eft>      
#> 1     1 <0.25h x 4> <0.25h x 4> <0.25h x 4> <0.25h x 4>
#> 2     2 <0.25h x 4> <0.25h x 4> <0.25h x 4> <0.25h x 4>
#> 3     3 <0.25h x 4> <0.25h x 4> <0.25h x 4> <0.25h x 4>
# A tibble: 3 × 5
#      id coe_A1 coe_A2 coe_B1 coe_B2
#   <int>  <dbl>  <dbl>  <dbl>  <dbl>
# 1     1      1      4      7     10

# Each column inherits classes
class(df_unfolded$coe_A1)
#> [1] "eft"     "coe"     "numeric"
# [1] "eft" "coe"

# Unfold without prefix
unfold(df_folded, coe, .prefix = "")
#> # A tibble: 3 × 5
#>      id A1          A2          B1          B2         
#>   <int> <eft>       <eft>       <eft>       <eft>      
#> 1     1 <0.25h x 4> <0.25h x 4> <0.25h x 4> <0.25h x 4>
#> 2     2 <0.25h x 4> <0.25h x 4> <0.25h x 4> <0.25h x 4>
#> 3     3 <0.25h x 4> <0.25h x 4> <0.25h x 4> <0.25h x 4>
# Creates: A1, A2, B1, B2

# Custom prefix
unfold(df_folded, coe, .prefix = "harm_")
#> # A tibble: 3 × 5
#>      id harm_A1     harm_A2     harm_B1     harm_B2    
#>   <int> <eft>       <eft>       <eft>       <eft>      
#> 1     1 <0.25h x 4> <0.25h x 4> <0.25h x 4> <0.25h x 4>
#> 2     2 <0.25h x 4> <0.25h x 4> <0.25h x 4> <0.25h x 4>
#> 3     3 <0.25h x 4> <0.25h x 4> <0.25h x 4> <0.25h x 4>
```
