# Identify coo columns in a tibble

Detect which columns in a tibble/data.frame contain coo objects
(coordinate matrices) or list columns of matrices.

## Usage

``` r
get_coo_cols(df, .cols = NULL)

get_all_coo_cols(df)
```

## Arguments

- df:

  A tibble or data.frame.

- .cols:

  Character vector, integer, logical, or NULL. If specified, use these
  column names/positions. If NULL, auto-detect columns containing coo
  objects or matrices.

## Value

- `get_coo_cols()`: Character string with single column name

- `get_all_coo_cols()`: Character vector with all coo column names

## Details

### Detection strategy

Both functions look for columns that contain coordinate data (matrices).
Detection priority:

1.  Columns with class "coo" (list of matrices with coo class)

2.  List columns where all elements are matrices

### Differences between functions

- **`get_coo_cols()`**: Returns a single column name. Errors if:

  - No coo columns found

  - Multiple coo columns found (user must specify `.cols`)

- **`get_all_coo_cols()`**: Returns all coo column names as a vector.
  Errors only if:

  - No coo columns found

### Manual specification

If automatic detection fails or you want to use a specific column, use
the `.cols` argument:

    get_coo_cols(df, .cols = "VD")
    get_coo_cols(df, .cols = 1)  # First column

## See also

[`get_coe_cols()`](https://momx.github.io/Momocs2/reference/get_coe_cols.md),
[`get_all_coe_cols()`](https://momx.github.io/Momocs2/reference/get_coe_cols.md)

## Examples

``` r
# Single coo column dataset
data(bot)
get_coo_cols(bot)
#> [1] "coo"
# [1] "coo"

# Multiple coo columns dataset
data(olea)
get_all_coo_cols(olea)
#> [1] "VD" "VL"
# [1] "VD" "VL"

# get_coo_cols() errors with multiple columns
try(get_coo_cols(olea))
#> Error in get_coo_cols(olea) : 
#>   Multiple coo columns found: VD, VL. Specify '.cols' to choose which to process.
# Error: Multiple coo columns found: VD, VL

# But can specify explicitly
get_coo_cols(olea, .cols = "VD")
#> [1] "VD"
# [1] "VD"

# Use with morphometric functions
olea %>%
  opoly(.cols = get_coo_cols(olea, "VD"))
#> Using degree = 5 (default)
#> # A tibble: 90 × 6
#>    id    VD        VL        var   status VD_coe 
#>    <chr> <cur>     <cur>     <fct> <fct>  <opoly>
#>  1 Ag1   (99 x 2)  (96 x 2)  Aglan cult   6 op   
#>  2 Ag2   (95 x 2)  (104 x 2) Aglan cult   6 op   
#>  3 Ag3   (101 x 2) (94 x 2)  Aglan cult   6 op   
#>  4 Ag4   (100 x 2) (93 x 2)  Aglan cult   6 op   
#>  5 Ag5   (102 x 2) (108 x 2) Aglan cult   6 op   
#>  6 Ag6   (98 x 2)  (93 x 2)  Aglan cult   6 op   
#>  7 Ag7   (100 x 2) (107 x 2) Aglan cult   6 op   
#>  8 Ag8   (100 x 2) (93 x 2)  Aglan cult   6 op   
#>  9 Ag9   (99 x 2)  (94 x 2)  Aglan cult   6 op   
#> 10 Ag10  (100 x 2) (95 x 2)  Aglan cult   6 op   
#> # ℹ 80 more rows
```
