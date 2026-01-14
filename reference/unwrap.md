# Unwrap coordinates from list-column of matrices to rows

Converts a list-column of coordinate matrices into separate x and y
columns, with one row per coordinate point.

## Usage

``` r
unwrap(.data, col, .id = NULL)
```

## Arguments

- .data:

  A tibble with a list-column containing coordinate matrices

- col:

  Column name of the list-column to unwrap (unquoted). If missing,
  auto-detects the `coo` column.

- .id:

  Optional name for an ID column to identify which matrix each row came
  from. If `NULL` (default) and no other identifying columns exist, adds
  a column named `".id"` automatically. Set to `FALSE` to never add an
  ID column.

## Value

A tibble with the list-column removed and replaced by `x` and `y`
columns (one row per coordinate point). An ID column is added if needed
to track which matrix each row belongs to.

## Details

The function converts each matrix in the list-column to a tibble with x
and y columns, then unnests to create one row per coordinate point. This
is the inverse of
[`wrap()`](https://momx.github.io/Momocs2/reference/wrap.md).

ID column behavior:

- If `.data` has other columns besides the list-column: uses those for
  identification

- If `.data` has ONLY the list-column: adds `.id` column automatically
  (or uses custom name)

- Set `.id = FALSE` to never add an ID column (may create ambiguous
  data)

## See also

[`wrap()`](https://momx.github.io/Momocs2/reference/wrap.md) for the
reverse operation

## Examples

``` r
# Start with wrapped coordinates
df_wrapped <- tibble::tibble(
  id = 1:3,
  coo = list(
    matrix(c(0, 1, 1, 0, 0, 0, 1, 1), ncol = 2, dimnames = list(NULL, c("x", "y"))),
    matrix(c(2, 3, 3, 2, 0, 0, 1, 1), ncol = 2, dimnames = list(NULL, c("x", "y"))),
    matrix(c(4, 5, 5, 4, 0, 0, 1, 1), ncol = 2, dimnames = list(NULL, c("x", "y")))
  )
)

# Unwrap to x, y rows
df_unwrapped <- df_wrapped %>% unwrap(coo)
#> Error in loadNamespace(x): there is no package called ‘tidyr’
df_unwrapped
#> Error: object 'df_unwrapped' not found
# A tibble: 12 × 3
#       id     x     y
#    <int> <dbl> <dbl>
#  1     1     0     0
#  2     1     1     0
#  3     1     1     1
#  4     1     0     1
# ...

# Auto-detect coo column
df_wrapped %>% unwrap()
#> Error in unwrap(.): No 'coo' column found. Please specify the column explicitly.

# Custom ID column name
tibble::tibble(coo = list(matrix(1:4, ncol = 2))) %>%
  unwrap(coo, .id = "shape_id")
#> Error in loadNamespace(x): there is no package called ‘tidyr’
# A tibble: 2 × 3
#   shape_id     x     y
#      <int> <int> <int>
# 1        1     1     3
# 2        1     2     4

# No ID column (not recommended unless you have other identifying columns)
df_wrapped %>% unwrap(coo, .id = FALSE)
#> Error in loadNamespace(x): there is no package called ‘tidyr’
```
