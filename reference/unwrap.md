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
bot %>% unwrap()
#> # A tibble: 6,469 × 6
#>    id         x     y type   fake  price
#>    <chr>  <dbl> <dbl> <fct>  <fct> <dbl>
#>  1 brahma    37   561 whisky a         3
#>  2 brahma    40   540 whisky a         3
#>  3 brahma    40   529 whisky a         3
#>  4 brahma    43   508 whisky a         3
#>  5 brahma    46   487 whisky a         3
#>  6 brahma    48   477 whisky a         3
#>  7 brahma    52   456 whisky a         3
#>  8 brahma    54   435 whisky a         3
#>  9 brahma    57   414 whisky a         3
#> 10 brahma    59   403 whisky a         3
#> # ℹ 6,459 more rows

bot %>% unwrap() %>% wrap()
#> # A tibble: 40 × 5
#>    id          type   fake  price coo      
#>    <chr>       <fct>  <fct> <dbl> <coo>    
#>  1 amrut       beer   c       3.5 (191 x 2)
#>  2 ballantines beer   c       2.2 (146 x 2)
#>  3 brahma      whisky a       3   (138 x 2)
#>  4 bushmills   beer   c       1.2 (165 x 2)
#>  5 caney       whisky a       1.2 (168 x 2)
#>  6 chimay      whisky a       3.8 (189 x 2)
#>  7 chivas      beer   c       3.9 (164 x 2)
#>  8 corona      whisky a       2.6 (129 x 2)
#>  9 dalmore     beer   c       1.6 (155 x 2)
#> 10 deusventrue whisky a       1.1 (152 x 2)
#> # ℹ 30 more rows
```
