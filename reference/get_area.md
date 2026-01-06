# Get area of a closed outline

Calculate the area enclosed by a shape using the shoelace formula.

## Usage

``` r
get_area(x, ..., .cols = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

## Value

- If `x` is a single matrix: returns a numeric scalar

- If `x` is a list: returns a numeric vector

- If `x` is a tibble: returns a numeric vector extracted from coo column

## Details

Uses the shoelace formula (also called surveyor's formula) to compute
area. The outline is automatically treated as closed (first point
connects to last).

For tibbles, this function extracts values without modifying the tibble.
Use within `mutate()` to add as a column, or use `measure("area")` for
convenience.

## See also

[`measure()`](https://momx.github.io/Momocs2/reference/measure.md) for
adding measurement columns;
[`get_perim()`](https://momx.github.io/Momocs2/reference/get_perim.md)
for perimeter

## Examples

``` r
get_area(shapes$cat)
#> [1] 14125.5
get_area(shapes)
#> $cat
#> [1] 14125.5
#> 
#> $dog
#> [1] 14907.5
#> 
#> $heart
#> [1] 24277
#> 
#> $leaf2
#> [1] 16905.5
#> 

# Extract from tibble
areas <- get_area(bot)

# Add to tibble
bot$area <- get_area(bot)

# Or use measure() for convenience
bot %>% measure("area")
#> # A tibble: 40 × 5
#>    coo       type   dummy    area coo_area
#>    <out>     <fct>  <fct>   <dbl>    <dbl>
#>  1 (138 x 2) whisky a     234515   234515 
#>  2 (168 x 2) whisky a     201056.  201056.
#>  3 (189 x 2) whisky a     119460.  119460.
#>  4 (129 x 2) whisky a     119568.  119568.
#>  5 (152 x 2) whisky a     165736.  165736.
#>  6 (161 x 2) whisky a     114015   114015 
#>  7 (124 x 2) whisky a     149503   149503 
#>  8 (126 x 2) whisky a     147642.  147642.
#>  9 (183 x 2) whisky a     130178.  130178.
#> 10 (193 x 2) whisky a     219548   219548 
#> # ℹ 30 more rows
```
