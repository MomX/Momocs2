# Fold multiple columns into a single list-column

Combines multiple related columns (e.g., Fourier coefficients A1, A2,
..., B1, B2, ...) into a single list-column where each row contains a
numeric vector of all the values. This is useful for storing
morphometric coefficients compactly in a tibble.

## Usage

``` r
fold(.data, ..., .class = NULL)
```

## Arguments

- .data:

  A data frame or tibble

- ...:

  A single named argument using tidyselect syntax. The name becomes the
  new list-column name, and the selection specifies which columns to
  fold. Must be exactly one named argument.

- .class:

  Character vector of classes to add to the list-column and individual
  vectors. If `NULL` (default), no classes are added - the list-column
  remains a plain list. For morphometric coefficients, specify classes
  explicitly, e.g., `.class = c("eft", "coe")` or `.class = "coe"`.

## Value

A tibble with selected columns removed and replaced by a single
list-column containing numeric vectors (one per row). If `.class` is
provided, both the list-column and individual vectors get those classes
(e.g., `c("eft", "coe", "list")` and `c("eft", "coe", "numeric")`). If
`.class = NULL`, returns a plain list-column.

## Details

The function:

1.  Selects columns using tidyselect (e.g., `A1:D5`,
    `starts_with("harm")`)

2.  Converts them to a matrix

3.  Splits the matrix by rows to create a list of numeric vectors

4.  Preserves original column names as names in each vector

5.  If `.class` is provided, adds classes to both the list-column and
    individual vectors

6.  Removes the original columns and adds the new list-column

This is particularly useful after Fourier analysis where you have many
harmonic coefficient columns that you want to store together. For
morphometric coefficients, always specify `.class` explicitly to ensure
proper class attributes.

## See also

[`unfold()`](https://momx.github.io/Momocs2/reference/unfold.md) for the
reverse operation,
[`get_coe_cols()`](https://momx.github.io/Momocs2/reference/get_coe_cols.md)
for detection

## Examples

``` r
# Create sample data with coefficient columns
df <- tibble::tibble(
  id = 1:3,
  A1 = c(1, 2, 3),
  A2 = c(4, 5, 6),
  B1 = c(7, 8, 9),
  B2 = c(10, 11, 12)
)

# Fold coefficient columns into a single list-column
df_folded <- fold(df, coe = A1:B2)  # Plain list, no classes
df_folded$coe[[1]]
#> A1 A2 B1 B2 
#>  1  4  7 10 
#    A1    A2    B1    B2
#     1     4     7    10

# For morphometric coefficients, specify classes explicitly
df_eft <- fold(df, coe = A1:B2, .class = c("eft", "coe"))

# List-column has classes only if specified
class(df_eft$coe)
#> [1] "eft"  "coe"  "list"
# [1] "eft"  "coe"  "list"

# Each element also has classes
class(df_eft$coe[[1]])
#> [1] "eft"     "coe"     "numeric"
# [1] "eft"     "coe"     "numeric"
names(df_eft$coe[[1]])
#> [1] "A1" "A2" "B1" "B2"
# [1] "A1" "A2" "B1" "B2"

# Without .class, just a plain list
df_plain <- fold(df, scores = A1:B2)
class(df_plain$scores)
#> [1] "array"
# [1] "list"
```
