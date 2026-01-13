# Coerce to Momocs morphometric classes

Add class attributes to objects for proper S3 dispatch and printing in
morphometric workflows.

## Usage

``` r
as_coo(x)

as_out(x)

as_ldk(x)

as_xy(x)

as_ldk_id(x)

as_cur(x)

as_path(x)

as_meas(x)

as_coe(x)

as_eft(x)

as_rft(x)

as_dct(x)

as_npoly(x)

as_opoly(x)

as_proc(x)
```

## Arguments

- x:

  An object to coerce (typically a matrix, vector, or list).

## Value

Object with updated class attribute.

## Details

These functions add Momocs-specific class attributes that enable:

- Custom printing methods

- Pretty display in tibbles (via pillar)

- S3 method dispatch for morphometric operations

- Type checking and validation

### Coordinate classes (inherit from `"coo"`)

Used for shape coordinate data, typically stored as nx2 matrices or
list-columns of matrices:

- **`as_coo()`**: Generic coordinate object - base class for all
  coordinate types

- **`as_out()`**: Closed outlines - class `c("out", "coo")`. For shapes
  where the first and last points connect (e.g., leaf outlines, bottle
  silhouettes)

- **`as_ldk()`**: Landmarks - class `c("ldk", "coo")`. For discrete
  anatomical points (e.g., skull landmarks, wing vein intersections)

- **`as_cur()`**: Open curves - class `c("cur", "coo")`. For shapes with
  distinct start and end points (e.g., leaf midribs, antenna segments)

### Single coordinate matrix

- **`as_xy()`**: Single coordinate matrix - class `"xy"`. Used for
  individual shape matrices (not list-columns). Has custom print method
  showing dimensions

### Landmark identifiers

- **`as_ldk_id()`**: Landmark indices - class `"ldk_id"`. Integer
  vectors indicating which points are landmarks. Does NOT inherit from
  `"coo"` since it contains indices, not coordinates

### Path metadata

- **`as_path()`**: File paths - class `"path"`. Character vectors of
  image file paths, typically used to track source images for shapes

### Measurement data

- **`as_meas()`**: Measurements - class `"meas"`. Numeric vectors of
  shape measurements (area, perimeter, etc.)

### Coefficient classes (all inherit from `"coe"`)

Used for shape descriptors from various decomposition methods:

- **`as_coe()`**: Generic coefficient object - class `"coe"`. Base class
  for all coefficient types

- **`as_eft()`**: Elliptic Fourier coefficients - class
  `c("eft", "coe")`. From closed outline decomposition (outlines)

- **`as_rft()`**: Radii variation Fourier coefficients - class
  `c("rft", "coe")`. From radii-based decomposition (outlines)

- **`as_dct()`**: Discrete Cosine Transform coefficients - class
  `c("dct", "coe")`. From open curve decomposition (curves)

- **`as_npoly()`**: Natural polynomial coefficients - class
  `c("npoly", "coe")`. From polynomial fitting (curves)

- **`as_opoly()`**: Orthogonal polynomial coefficients - class
  `c("opoly", "coe")`. From orthogonal polynomial fitting (curves)

- **`as_proc()`**: Procrustes coefficients - class `c("proc", "coe")`.
  From Procrustes superimposition (landmarks)

### Class hierarchy

    coo (coordinates)
    ├── out (closed outlines)
    ├── ldk (landmarks)
    └── cur (open curves)

    coe (coefficients)
    ├── eft (elliptic Fourier - outlines)
    ├── rft (radii Fourier - outlines)
    ├── dct (discrete cosine - curves)
    ├── npoly (natural polynomial - curves)
    ├── opoly (orthogonal polynomial - curves)
    └── proc (Procrustes - landmarks)

    xy (single coordinate matrix)
    ldk_id (landmark indices)
    path (file paths)
    meas (measurements)

## See also

[`declass()`](https://momx.github.io/Momocs2/reference/declass.md) to
remove Momocs classes

## Examples

``` r
# Coordinate classes
mat <- matrix(rnorm(100), ncol = 2)
coo <- as_coo(mat)
class(coo)  # "coo" "matrix" "array"
#> [1] "coo"    "matrix" "array" 

outline <- as_out(mat)
class(outline)  # "out" "coo" "matrix" "array"
#> [1] "out"    "coo"    "matrix" "array" 

landmarks <- as_ldk(mat)
class(landmarks)  # "ldk" "coo" "matrix" "array"
#> [1] "ldk"    "coo"    "matrix" "array" 

# Coefficient classes
coefs <- rnorm(24)  # 6 harmonics × 4 coefficients
eft_coefs <- as_eft(coefs)
class(eft_coefs)  # "eft" "coe" "numeric"
#> [1] "eft"     "coe"     "numeric"

# Landmark identifiers (NOT coo)
ldk_indices <- as_ldk_id(c(1, 5, 10, 25))
class(ldk_indices)  # "ldk_id" "integer"
#> [1] "ldk_id"  "numeric"

# In tibbles
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
tibble(
  shape = list(mat, mat * 2),
  coef = list(coefs, coefs * 2)
) %>%
  mutate(
    shape = as_out(shape),
    coef = as_eft(coef)
  )
#> # A tibble: 2 × 2
#>   shape    coef    
#>   <out>    <eft>   
#> 1 (50 x 2) <6h x 4>
#> 2 (50 x 2) <6h x 4>
```
