# Unfold a list-column into multiple columns or a matrix

Expands a list-column of numeric vectors into separate columns (when
applied to a tibble) or into a matrix (when applied to a list-column
directly). This is the inverse operation of
[`fold()`](https://momx.github.io/Momocs2/reference/fold.md).

## Usage

``` r
unfold(x, ...)

# S3 method for class 'data.frame'
unfold(x, col, .prefix = NULL, ...)

# S3 method for class 'list'
unfold(x, .prefix = NULL, ...)
```

## Arguments

- x:

  A data frame, tibble, or list of numeric vectors

- ...:

  Additional arguments passed to methods

- col:

  Name of the list-column to unfold (unquoted). If missing,
  automatically detects the single coe column using
  [`get_coe_cols()`](https://momx.github.io/Momocs2/reference/get_coe_cols.md).

- .prefix:

  Character string to prefix column names. Default `NULL` means no
  prefix. Set to a string to add a prefix to all column names.

## Value

- For tibbles: A tibble with the list-column removed and replaced by
  multiple numeric columns

- For lists: A numeric matrix with one row per list element

## Details

`unfold()` is an S3 generic with methods for:

- **Data frames/tibbles** (`unfold.data.frame`): User-facing, expands
  list-columns into separate columns with prefixes and class
  preservation

- **Lists** (`unfold.list`): Internal use, converts list of vectors
  directly to a matrix for statistical functions

### For data frames (tibbles)

The function:

1.  Auto-detects the coe column if not specified

2.  Extracts the list-column and its classes

3.  Stacks all vectors into a matrix (by rows)

4.  Converts the matrix to a tibble with prefixed column names

5.  Copies classes from the list-column to each new column

6.  Removes the original list-column and binds the new columns

This is useful when you need to access individual coefficient values for
statistical analysis, plotting, or further manipulation.

### For lists

When applied directly to a list of numeric vectors, `unfold()` converts
it to a matrix by stacking vectors as rows. This is useful for internal
operations that need matrix input (e.g.,
[`prcomp()`](https://rdrr.io/r/stats/prcomp.html), `lda()`).

The function:

1.  Finds the first non-NA element to determine structure

2.  Extracts or generates column names

3.  Stacks all vectors into a matrix (one row per vector)

4.  Preserves names as column names

## See also

`unfold.data.frame()`, `unfold.list()`,
[`fold()`](https://momx.github.io/Momocs2/reference/fold.md)

## Examples

``` r
# For tibbles - see ?unfold.data.frame
df_folded <- tibble::tibble(
  id = 1:3,
  coe = list(c(A1=1, A2=4), c(A1=2, A2=5), c(A1=3, A2=6))
)
unfold(df_folded, coe)
#> # A tibble: 3 × 3
#>      id coe_A1 coe_A2
#>   <int>  <dbl>  <dbl>
#> 1     1      1      4
#> 2     2      2      5
#> 3     3      3      6

# For lists directly (returns matrix)
coe_list <- list(c(A1=1, A2=4), c(A1=2, A2=5), c(A1=3, A2=6))
unfold(coe_list)
#>      A1 A2
#> [1,]  1  4
#> [2,]  2  5
#> [3,]  3  6
#      A1 A2
# [1,]  1  4
# [2,]  2  5
# [3,]  3  6

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
#>      id coe_A1 coe_A2 coe_B1 coe_B2
#>   <int>  <dbl>  <dbl>  <dbl>  <dbl>
#> 1     1      1      4      7     10
#> 2     2      2      5      8     11
#> 3     3      3      6      9     12
# A tibble: 3 × 5
#      id coe_A1 coe_A2 coe_B1 coe_B2
#   <int>  <dbl>  <dbl>  <dbl>  <dbl>
# 1     1      1      4      7     10

# Each column inherits classes
class(df_unfolded$coe_A1)
#> [1] "numeric"
# [1] "eft" "coe"

# Unfold without prefix
unfold(df_folded, coe, .prefix = "")
#> # A tibble: 3 × 5
#>      id    A1    A2    B1    B2
#>   <int> <dbl> <dbl> <dbl> <dbl>
#> 1     1     1     4     7    10
#> 2     2     2     5     8    11
#> 3     3     3     6     9    12
# Creates: A1, A2, B1, B2

# Custom prefix
unfold(df_folded, coe, .prefix = "harm_")
#> # A tibble: 3 × 5
#>      id harm_A1 harm_A2 harm_B1 harm_B2
#>   <int>   <dbl>   <dbl>   <dbl>   <dbl>
#> 1     1       1       4       7      10
#> 2     2       2       5       8      11
#> 3     3       3       6       9      12

# Direct list to matrix conversion
coe_list <- list(
  c(A1 = 1, A2 = 4, B1 = 7),
  c(A1 = 2, A2 = 5, B1 = 8),
  c(A1 = 3, A2 = 6, B1 = 9)
)

mat <- unfold(coe_list)
mat
#>      A1 A2 B1
#> [1,]  1  4  7
#> [2,]  2  5  8
#> [3,]  3  6  9
#      A1 A2 B1
# [1,]  1  4  7
# [2,]  2  5  8
# [3,]  3  6  9

# With prefix
unfold(coe_list, .prefix = "coef_")
#>      coef_A1 coef_A2 coef_B1
#> [1,]       1       4       7
#> [2,]       2       5       8
#> [3,]       3       6       9
#      coef_A1 coef_A2 coef_B1
# [1,]       1       4       7
```
