# List available measurements

Show all measurements that can be computed with
[`measure()`](https://momx.github.io/Momocs2/reference/measure.md).

## Usage

``` r
available_measures()
```

## Value

Character vector of available measurement names.

## Details

Returns names of all scalar-returning `get_*` functions that can be used
with [`measure()`](https://momx.github.io/Momocs2/reference/measure.md).

## Examples

``` r
available_measures()
#>  [1] "area"                 "perim"                "centroid_size"       
#>  [4] "centroid_size_norm"   "circularity"          "circularity_norm"    
#>  [7] "circularity_haralick" "length"               "width"               
#> [10] "elongation"           "rectangularity"       "convexity"           
#> [13] "solidity"             "rectilinearity"       "calliper"            
```
