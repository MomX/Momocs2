# Identify coe columns in a tibble

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

- Columns with class `c("opoly", "coe", "list")`

- Columns with class `c("npoly", "coe", "list")`

- Columns with class `c("dct", "coe", "list")`

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

    get_coe_cols(df, .cols = "VD_coe")
    get_coe_cols(df, .cols = 2)  # Second column

## See also

[`get_coo_cols()`](https://momx.github.io/Momocs2/reference/get_coo_cols.md),
[`get_all_coo_cols()`](https://momx.github.io/Momocs2/reference/get_coo_cols.md),
[`opoly()`](https://momx.github.io/Momocs2/reference/opoly.md),
[`npoly()`](https://momx.github.io/Momocs2/reference/npoly.md),
[`dct()`](https://momx.github.io/Momocs2/reference/dct.md),
[`eft()`](https://momx.github.io/Momocs2/reference/eft.md)

## Examples

``` r
# Single coe column
data(bot)
bot_eft <- bot %>% eft()
#> Using coo column: 'coo'
#> Using nb_h = 6 harmonics (default)
get_coe_cols(bot_eft)
#> [1] "coe"
# [1] "coe"

# Multiple coe columns
data(olea)
olea_poly <- olea %>% opoly()
#> Processing coo column(s): VD, VL
#> Using degree = 5 (default)
get_all_coe_cols(olea_poly)
#> [1] "VD_coe" "VL_coe"
# [1] "VD_coe" "VL_coe"

# get_coe_cols() errors with multiple
try(get_coe_cols(olea_poly))
#> Error in get_coe_cols(olea_poly) : 
#>   Multiple coe columns found: VD_coe, VL_coe. Use .cols to specify which one.
# Error: Multiple coe columns found: VD_coe, VL_coe

# But can specify explicitly
get_coe_cols(olea_poly, .cols = "VD_coe")
#> [1] "VD_coe"
# [1] "VD_coe"

# Use with inverse transforms
olea %>%
  opoly() %>%
  opoly_i(.cols = get_coe_cols(., "VD_coe"))
#> Processing coo column(s): VD, VL
#> Using degree = 5 (default)
#> # A tibble: 90 × 8
#>    id    VD        VL        var   status VD_coe  VL_coe  VD_coe_i 
#>    <chr> <cur>     <cur>     <fct> <fct>  <opoly> <opoly> <opn>    
#>  1 Ag1   (99 x 2)  (96 x 2)  Aglan cult   6 op    6 op    (120 x 2)
#>  2 Ag2   (95 x 2)  (104 x 2) Aglan cult   6 op    6 op    (120 x 2)
#>  3 Ag3   (101 x 2) (94 x 2)  Aglan cult   6 op    6 op    (120 x 2)
#>  4 Ag4   (100 x 2) (93 x 2)  Aglan cult   6 op    6 op    (120 x 2)
#>  5 Ag5   (102 x 2) (108 x 2) Aglan cult   6 op    6 op    (120 x 2)
#>  6 Ag6   (98 x 2)  (93 x 2)  Aglan cult   6 op    6 op    (120 x 2)
#>  7 Ag7   (100 x 2) (107 x 2) Aglan cult   6 op    6 op    (120 x 2)
#>  8 Ag8   (100 x 2) (93 x 2)  Aglan cult   6 op    6 op    (120 x 2)
#>  9 Ag9   (99 x 2)  (94 x 2)  Aglan cult   6 op    6 op    (120 x 2)
#> 10 Ag10  (100 x 2) (95 x 2)  Aglan cult   6 op    6 op    (120 x 2)
#> # ℹ 80 more rows
```
