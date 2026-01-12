# Identify coe columns in a tibble

Detect which columns in a tibble/data.frame contain coefficient objects
or list columns of numeric vectors/matrices.

## Usage

``` r
get_coe_cols(df, .cols = NULL)
```

## Arguments

- df:

  A tibble or data.frame.

- .cols:

  Character vector or NULL. If specified, use these column names. If
  NULL, auto-detect columns containing coe objects.

## Value

Character vector of column names to process.

## Details

Detection priority:

1.  Columns with class "coe" in their class hierarchy (e.g., c("eft",
    "coe", "list"))

2.  List columns where all elements are numeric vectors or matrices

When multiple qualifying columns exist, an error is raised and user must
specify `.cols` explicitly.

## See also

[`get_coo_cols()`](https://momx.github.io/Momocs2/reference/get_coo_cols.md)
for coordinate columns

## Examples

``` r
#bot %>% eft() %>% get_coe_cols
#get_coe_cols(boteft, "coe")
```
