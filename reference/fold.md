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
  vectors. If `NULL` (default), uses the new column name. For example,
  if folding into a column named "coe", both the list-column and
  individual vectors will get classes `c("coe", "list")` and
  `c("coe", "numeric")` respectively.

## Value

A tibble with selected columns removed and replaced by a single
list-column containing classed numeric vectors (one per row). The
list-column itself gets class `c(.class, "list")` and each vector gets
`c(.class, "numeric")`.

## Details

The function:

1.  Selects columns using tidyselect (e.g., `A1:D5`,
    `starts_with("harm")`)

2.  Converts them to a matrix

3.  Splits the matrix by rows to create a list of numeric vectors

4.  Preserves original column names as names in each vector

5.  Adds appropriate classes to both the list-column and individual
    vectors

6.  Removes the original columns and adds the new list-column

This is particularly useful after Fourier analysis where you have many
harmonic coefficient columns that you want to store together as a single
"coe" object.

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
df_folded <- fold(df, coe = A1:B2)
df_folded$coe[[1]]
#> A1 A2 B1 B2 
#>  1  4  7 10 
#> attr(,"class")
#> [1] "coe"     "numeric"
#    A1    A2    B1    B2
#     1     4     7    10

# List-column has classes
class(df_folded$coe)
#> [1] "coe"  "list"
# [1] "coe"  "list"

# Each element also has classes and preserves names
class(df_folded$coe[[1]])
#> [1] "coe"     "numeric"
# [1] "coe"     "numeric"
names(df_folded$coe[[1]])
#> [1] "A1" "A2" "B1" "B2"
# [1] "A1" "A2" "B1" "B2"

# Can specify custom classes (e.g., for efourier)
fold(df, coe = A1:B2, .class = c("eft", "coe"))
#> # A tibble: 3 × 2
#>      id coe  
#>   <int> <eft>
#> 1     1 <NA> 
#> 2     2 <NA> 
#> 3     3 <NA> 
```
