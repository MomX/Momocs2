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
Use within
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) to add
as a column, or use `measure("area")` for convenience.

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
#> [1] 14125.5 14907.5 24277.0 16905.5

# Extract from tibble
areas <- get_area(bot)

# Add to tibble
bot$area <- get_area(bot)

# Or use measure() for convenience
bot %>% measure("area")
#> # A tibble: 40 × 7
#>    id           coo       type   fake  price    area coo_area
#>    <chr>        <out>     <fct>  <fct> <dbl>   <dbl>    <dbl>
#>  1 brahma       (138 x 2) whisky a       3   234515   234515 
#>  2 caney        (168 x 2) whisky a       1.2 201056.  201056.
#>  3 chimay       (189 x 2) whisky a       3.8 119460.  119460.
#>  4 corona       (129 x 2) whisky a       2.6 119568.  119568.
#>  5 deusventrue  (152 x 2) whisky a       1.1 165736.  165736.
#>  6 duvel        (161 x 2) whisky a       3.1 114015   114015 
#>  7 franziskaner (124 x 2) whisky a       2.6 149503   149503 
#>  8 grimbergen   (126 x 2) whisky a       2.9 147642.  147642.
#>  9 guiness      (183 x 2) whisky a       1.2 130178.  130178.
#> 10 hoegardeen   (193 x 2) whisky a       3.6 219548   219548 
#> # ℹ 30 more rows
```
