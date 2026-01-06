# Pipe operators

Momocs2 uses magrittr's pipe operators for clear, readable workflows.

## Arguments

- lhs:

  A value or the magrittr placeholder.

- rhs:

  A function call using the magrittr semantics.

## Details

**Forward pipe (`%>%`)**: Passes result forward to next function

    bot %>%
      coo_center() %>%
      coo_scale() %>%
      measure(c("area", "circularity"))

**Tee pipe (`%T>%`)**: Passes input forward unchanged (useful for side
effects like plotting)

    bot %>%
      coo_center() %T>%
      pile() %>%           # Plot without breaking the chain
      coo_scale()

**Exposition pipe (`%$%`)**: Exposes column names for direct use

    bot %>%
      measure(c("area", "perim")) %$%
      plot(coo_area, coo_perim)  # Use columns directly without bot$

## See also

`magrittr` vignettes for detailed documentation.

## Examples

``` r
# Forward pipe - standard workflow.
# Almost exactly equivalent to (now native) |>
bot %>%
  coo_center() %>%
  coo_scale() %>%
  measure("area")
#> # A tibble: 40 × 4
#>    coo       type   dummy coo_area
#>    <out>     <fct>  <fct>    <dbl>
#>  1 (138 x 2) whisky a         1.77
#>  2 (168 x 2) whisky a         1.82
#>  3 (189 x 2) whisky a         2.21
#>  4 (129 x 2) whisky a         1.67
#>  5 (152 x 2) whisky a         1.84
#>  6 (161 x 2) whisky a         2.35
#>  7 (124 x 2) whisky a         1.78
#>  8 (126 x 2) whisky a         2.05
#>  9 (183 x 2) whisky a         1.98
#> 10 (193 x 2) whisky a         1.76
#> # ℹ 30 more rows

# Tee pipe - plot without breaking chain
bot %>%
  coo_center() %T>%
  pile() %>%           # Plot without breaking the chain
  coo_scale()

#> # A tibble: 40 × 3
#>    coo       type   dummy
#>    <out>     <fct>  <fct>
#>  1 (138 x 2) whisky a    
#>  2 (168 x 2) whisky a    
#>  3 (189 x 2) whisky a    
#>  4 (129 x 2) whisky a    
#>  5 (152 x 2) whisky a    
#>  6 (161 x 2) whisky a    
#>  7 (124 x 2) whisky a    
#>  8 (126 x 2) whisky a    
#>  9 (183 x 2) whisky a    
#> 10 (193 x 2) whisky a    
#> # ℹ 30 more rows

# Exposition pipe - access columns directly
bot %>%
  measure(c("area", "perim")) %$%
  plot(coo_area, coo_perim)
```
