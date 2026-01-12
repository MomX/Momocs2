# Create a get function with automatic dispatch

Higher-order function factory that creates get\_ functions with
automatic dispatch to handle single matrices, lists of matrices, and
tibbles.

## Usage

``` r
make_get_function(impl_fn, fn_name = NULL)
```

## Arguments

- impl_fn:

  Function. The implementation function that operates on a single matrix
  (nx2). Should accept `x` as first argument and `...` for additional
  args.

- fn_name:

  Character. Optional name for debugging (not used functionally).

## Value

A function that dispatches based on input type.

## Details

The returned function automatically:

- Applies impl_fn to single matrices and returns the result (as numeric
  if scalar)

- Applies impl_fn to each element of a list and returns a list
  (simplified to numeric vector if all results are scalars)

- Applies impl_fn to tibble coo columns and EXTRACTS results (simplified
  to numeric vector if all results are scalars)

For tibbles, get\_\* functions extract values for further processing by
the user. They do NOT create new columns or modify the tibble.

Simplification to numeric: If all results are length-1 scalars, the
output is converted from a list to a numeric vector for convenience.

Additional arguments are passed through via `...`
