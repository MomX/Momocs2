# Measure shape properties

Add measurement columns to a tibble by computing scalar shape
properties.

## Usage

``` r
measure(x, measures, ..., .cols = NULL, .prefix = NULL)
```

## Arguments

- x:

  A tibble with coo columns.

- measures:

  Character vector of measurement names to compute. See Details for
  available measurements.

- ...:

  Additional arguments passed to measurement functions.

- .cols:

  Column name(s) to process. If `NULL`, automatically detects columns
  containing coo objects.

- .prefix:

  Character. Custom prefix for new column names. If `NULL`, uses
  "colname_measurename" (e.g., "coo_area").

## Value

The input tibble with new measurement column(s) added.

## Details

Available measurements (all require scalar-returning `get_*` functions):

**Size and perimeter:**

- `"area"` - polygon area (shoelace formula)

- `"perim"` - perimeter length

**Centroid size:**

- `"centroid_size"` - centroid size (standard)

- `"centroid_size_norm"` - normalized centroid size

**Circularity:**

- `"circularity"` - isoperimetric quotient (4πA/P²)

- `"circularity_norm"` - normalized circularity

- `"circularity_haralick"` - mean(radii)/sd(radii) from centroid

**Length and aspect ratio:**

- `"length"` - range along major inertia axis

- `"width"` - range along minor inertia axis

- `"elongation"` - aspect ratio (length/width)

- `"rectangularity"` - area/(length×width)

**Convex hull measures:**

- `"convexity"` - perim_hull/perim

- `"solidity"` - area/area_hull

**Complex shape descriptors:**

- `"rectilinearity"` - rectangular fit after optimal rotation

- `"calliper"` - maximum distance between points

To see all available measurements, use
[`available_measures()`](https://momx.github.io/Momocs2/reference/available_measures.md).

## See also

[`available_measures()`](https://momx.github.io/Momocs2/reference/available_measures.md)
for list of measurements; `get_*` functions for extraction

## Examples

``` r
# Single measurement
bot %>% measure("area")
#> # A tibble: 40 × 6
#>    id           coo       type   fake  price coo_area
#>    <chr>        <out>     <fct>  <fct> <dbl>    <dbl>
#>  1 brahma       (138 x 2) whisky a       3    234515 
#>  2 caney        (168 x 2) whisky a       1.2  201056.
#>  3 chimay       (189 x 2) whisky a       3.8  119460.
#>  4 corona       (129 x 2) whisky a       2.6  119568.
#>  5 deusventrue  (152 x 2) whisky a       1.1  165736.
#>  6 duvel        (161 x 2) whisky a       3.1  114015 
#>  7 franziskaner (124 x 2) whisky a       2.6  149503 
#>  8 grimbergen   (126 x 2) whisky a       2.9  147642.
#>  9 guiness      (183 x 2) whisky a       1.2  130178.
#> 10 hoegardeen   (193 x 2) whisky a       3.6  219548 
#> # ℹ 30 more rows

# Multiple measurements
bot %>% measure(c("area", "perim", "centroid_size"))
#> # A tibble: 40 × 8
#>    id           coo       type  fake  price coo_area coo_perim coo_centroid_size
#>    <chr>        <out>     <fct> <fct> <dbl>    <dbl>     <dbl>             <dbl>
#>  1 brahma       (138 x 2) whis… a       3    234515      2514.             4277.
#>  2 caney        (168 x 2) whis… a       1.2  201056.     2289.             4312.
#>  3 chimay       (189 x 2) whis… a       3.8  119460.     1593.             3193.
#>  4 corona       (129 x 2) whis… a       2.6  119568.     1838.             3035.
#>  5 deusventrue  (152 x 2) whis… a       1.1  165736.     2087.             3701.
#>  6 duvel        (161 x 2) whis… a       3.1  114015      1509.             2796.
#>  7 franziskaner (124 x 2) whis… a       2.6  149503      1985.             3225.
#>  8 grimbergen   (126 x 2) whis… a       2.9  147642.     1848.             3011.
#>  9 guiness      (183 x 2) whis… a       1.2  130178.     1767.             3472.
#> 10 hoegardeen   (193 x 2) whis… a       3.6  219548      2424.             4907.
#> # ℹ 30 more rows

# Custom prefix
bot %>% measure(c("area", "perim"), .prefix = "shape")
#> # A tibble: 40 × 7
#>    id           coo       type   fake  price shape_area shape_perim
#>    <chr>        <out>     <fct>  <fct> <dbl>      <dbl>       <dbl>
#>  1 brahma       (138 x 2) whisky a       3      234515        2514.
#>  2 caney        (168 x 2) whisky a       1.2    201056.       2289.
#>  3 chimay       (189 x 2) whisky a       3.8    119460.       1593.
#>  4 corona       (129 x 2) whisky a       2.6    119568.       1838.
#>  5 deusventrue  (152 x 2) whisky a       1.1    165736.       2087.
#>  6 duvel        (161 x 2) whisky a       3.1    114015        1509.
#>  7 franziskaner (124 x 2) whisky a       2.6    149503        1985.
#>  8 grimbergen   (126 x 2) whisky a       2.9    147642.       1848.
#>  9 guiness      (183 x 2) whisky a       1.2    130178.       1767.
#> 10 hoegardeen   (193 x 2) whisky a       3.6    219548        2424.
#> # ℹ 30 more rows

# On specific column
bot %>% measure("area", .cols = coo)
#> # A tibble: 40 × 6
#>    id           coo       type   fake  price coo_area
#>    <chr>        <out>     <fct>  <fct> <dbl>    <dbl>
#>  1 brahma       (138 x 2) whisky a       3    234515 
#>  2 caney        (168 x 2) whisky a       1.2  201056.
#>  3 chimay       (189 x 2) whisky a       3.8  119460.
#>  4 corona       (129 x 2) whisky a       2.6  119568.
#>  5 deusventrue  (152 x 2) whisky a       1.1  165736.
#>  6 duvel        (161 x 2) whisky a       3.1  114015 
#>  7 franziskaner (124 x 2) whisky a       2.6  149503 
#>  8 grimbergen   (126 x 2) whisky a       2.9  147642.
#>  9 guiness      (183 x 2) whisky a       1.2  130178.
#> 10 hoegardeen   (193 x 2) whisky a       3.6  219548 
#> # ℹ 30 more rows
```
