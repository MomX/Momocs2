# Baseline correction (Bookstein registration)

Align shape using baseline/Bookstein registration with specified
landmarks.

## Usage

``` r
coo_baseline(x, id1 = 1, id2 = nrow(x), t1 = c(-0.5, 0), t2 = c(0.5, 0))

coo_bookstein(x, id1 = 1, id2 = nrow(x))
```

## Arguments

- x:

  A matrix (nx2) of coordinates.

- id1:

  Integer. Index of first landmark point.

- id2:

  Integer. Index of second landmark point.

- t1:

  Numeric vector of length 2. Target position for first landmark.

- t2:

  Numeric vector of length 2. Target position for second landmark.

## Value

A matrix with the baseline-corrected shape.

## Details

`coo_bookstein()` is an alias for `coo_baseline()` with default target
positions at (-0.5, 0) and (0.5, 0) for standardized alignment.

Note: These functions do not use the `make_coo_function` dispatcher as
they require specific landmark indices.
