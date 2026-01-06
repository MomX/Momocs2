# Create a coo function with automatic dispatch

Higher-order function factory that creates coo\_ functions with
automatic dispatch to handle single matrices, lists of matrices, and
tibbles.

## Usage

``` r
make_coo_function(impl_fn, fn_name = NULL, sync_ldk = FALSE)
```

## Arguments

- impl_fn:

  Function. The implementation function that operates on a single matrix
  (nx2). Should accept `x` as first argument and `...` for additional
  args.

- fn_name:

  Character. Optional name for debugging (not used functionally).

- sync_ldk:

  Logical. If TRUE, function is landmark-aware and will sync landmark
  columns automatically. Default is FALSE.

## Value

A function that dispatches based on input type.

## Details

The returned function automatically:

- Applies impl_fn to single matrices and returns the result

- Applies impl_fn to each element of a list and returns a list

- Applies impl_fn to specified columns of a tibble and returns the
  modified tibble

- Applies `xy` class to each single matrix

- Applies `coo` class to list and list-columns (ie when tibbles are
  passed)

When `sync_ldk = TRUE`, the dispatcher automatically handles landmark
columns:

- Looks for a column named `colname_ldk` (e.g., `coo_ldk` for `coo`
  column)

- Or uses `.ldk_col` argument if provided

- Passes landmarks to implementation function

- Updates landmarks based on coordinate changes

When `sync_ldk = FALSE`, the implementation function should return just
a matrix, not a list with coo and ldk elements.

Additional arguments are passed through via `...`
