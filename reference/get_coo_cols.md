# Identify coo columns in a tibble

Detect which columns in a tibble/data.frame contain coo objects or list
columns of matrices.

## Usage

``` r
get_coo_cols(df, .cols = NULL)
```

## Arguments

- df:

  A tibble or data.frame.

- .cols:

  Character vector or NULL. If specified, use these column names. If
  NULL, auto-detect columns containing coo objects or matrices.

## Value

Character vector of column names to process.

## Details

Detection priority:

1.  Columns with class "coo" (list of matrices with coo class)

2.  List columns where all elements are matrices

When multiple qualifying columns exist, an error is raised and user must
specify `.cols` explicitly.

## Examples

``` r
get_coo_cols(bot)
#> [1] "coo"
get_coo_cols(bot, "coo")
#> [1] "coo"
```
