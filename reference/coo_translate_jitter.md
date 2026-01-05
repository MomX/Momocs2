# Jitter coordinates

Add random translation to a shape.

## Usage

``` r
coo_translate_jitter(x, ..., .cols = NULL, .ldk_col = NULL)
```

## Arguments

- x:

  A matrix (nx2), list of matrices, or tibble with coo columns.

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coo objects.

- amount:

  Numeric. Maximum jitter amount. Default is 10% of normalized centroid
  size.

## Value

- If `x` is a single matrix: returns the jittered matrix

- If `x` is a list: returns a list of jittered matrices

- If `x` is a tibble: returns the tibble with specified coo columns
  jittered

## See also

[`coo_translate()`](https://momx.github.io/Momocs2/reference/coo_translate.md)
for deterministic translation
