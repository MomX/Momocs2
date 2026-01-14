# Identify coe columns in a tibble Get coefficient column name(s)

Detect and return the name of coefficient column(s) in a tibble. These
functions look for columns with class `"coe"` in their class hierarchy.

## Usage

``` r
get_coe_cols(df, .cols = NULL)

get_all_coe_cols(df)
```

## Arguments

- df:

  A data frame or tibble

- .cols:

  Optional. Explicitly specify which column(s) to use. Can be:

  - Character vector of column names

  - Integer vector of column positions

  - Logical vector If `NULL` (default), automatically detects columns
    with class `"coe"`.

## Value

- `get_coe_cols()`: Character string with single column name

- `get_all_coe_cols()`: Character vector with all coe column names

## Details

### Detection strategy

Both functions look for columns that have `"coe"` anywhere in their
class hierarchy. This includes:

- Columns with class `c("coe", "list")`

- Columns with class `c("eft", "coe", "list")`

- Columns with class `c("out", "coe", "list")`

- Any other combination where `"coe"` is present

All morphometric methods automatically add the `"coe"` class to their
output columns.

### Differences between functions

- **`get_coe_cols()`**: Returns a single column name. Errors if:

  - No `"coe"` columns found

  - Multiple `"coe"` columns found (user must specify `.cols`)

- **`get_all_coe_cols()`**: Returns all coe column names as a vector.
  Errors only if:

  - No `"coe"` columns found

### Manual specification

If automatic detection fails or you want to use a specific column, use
the `.cols` argument in `get_coe_cols()`:

    get_coe_cols(df, .cols = "my_coefficients")
    get_coe_cols(df, .cols = 2)  # Second column

## See also

[`fold()`](https://momx.github.io/Momocs2/reference/fold.md),
[`unfold()`](https://momx.github.io/Momocs2/reference/unfold.md),
[`efourier()`](https://momx.github.io/Momocs2/reference/efourier.md)

## Examples

``` r
# Create sample data with coefficient columns
df <- tibble::tibble(
  id = 1:3,
  coe = list(1:5, 2:6, 3:7)
)
class(df$coe) <- c("eft", "coe", "list")

# Get single coe column
get_coe_cols(df)
#> [1] "coe"
# [1] "coe"

# Multiple coe columns
df$coe2 <- df$coe
class(df$coe2) <- c("coe", "list")

# get_coe_cols() errors with multiple
try(get_coe_cols(df))
#> Error in get_coe_cols(df) : 
#>   Multiple coe columns found: coe, coe2. Use .cols to specify which one.
# Error: Multiple coe columns found: coe, coe2

# But can specify explicitly
get_coe_cols(df, .cols = "coe")
#> [1] "coe"
# [1] "coe"

# get_all_coe_cols() returns all
get_all_coe_cols(df)
#> [1] "coe"  "coe2"
# [1] "coe"  "coe2"
```
