# Normalize elliptic Fourier coefficients

Normalize EFT coefficients to remove the effects of size, rotation, and
starting position, making shapes directly comparable.

## Usage

``` r
eft_norm(x, start = FALSE, ..., .cols = NULL, .name = NULL)
```

## Arguments

- x:

  A numeric vector (EFT coefficients), list of vectors, or tibble with
  coe columns.

- start:

  Logical. If `TRUE`, preserves the starting point position. Default is
  `FALSE` (removes starting point effect).

- ...:

  Additional arguments (reserved for future use).

- .cols:

  Column name(s) to process when `x` is a tibble. If `NULL`,
  automatically detects columns containing coe objects.

- .name:

  Character. Name for the output coefficient column when `x` is a
  tibble. If `NULL`, modifies the column in place. If provided, creates
  a new column with this name.

## Value

- If `x` is a vector: returns a normalized coefficient vector with class
  `c("eft", "numeric")`

- If `x` is a list: returns a list of normalized coefficient vectors

- If `x` is a tibble: returns the tibble with normalized coefficients
  (either modifying in place or creating a new column)

## Details

Normalization removes three sources of variation that are typically not
of interest in shape analysis:

1.  **Size**: Coefficients are scaled so the first ellipse has unit
    semi-axis length

2.  **Rotation**: Coefficients are rotated so the first ellipse aligns
    with the x-axis

3.  **Starting point** (if `start = FALSE`): The phase of all harmonics
    is adjusted to remove the effect of where digitization began

After normalization, shapes can be directly compared using multivariate
statistics, as remaining variation represents only shape differences.

The first harmonic (A1, B1, C1, D1) is used to compute the normalization
parameters. For `start = FALSE`, these parameters are:

- `theta`: Phase shift to remove starting point effect

- `psi`: Rotation angle to align with x-axis

- `size`: Scaling factor (inverse of first ellipse semi-axis length)

These normalization parameters are stored as attributes on the result
for reference but are typically not needed for downstream analysis.

## When to use normalization

- **Always normalize** before multivariate analysis (PCA, LDA, etc.) if
  you want to compare shapes independent of size, rotation, and starting
  point

- **Skip normalization** if size or orientation are biologically
  meaningful

- Use `start = TRUE` if the starting point has biological meaning (e.g.,
  a specific landmark)

## References

Kuhl, F. P., & Giardina, C. R. (1982). Elliptic Fourier features of a
closed contour. Computer graphics and image processing, 18(3), 236-258.

## See also

[`eft()`](https://momx.github.io/Momocs2/reference/eft.md) for
coefficient computation,
[`eft_i()`](https://momx.github.io/Momocs2/reference/eft_i.md) for
reconstruction.

## Examples

``` r
# Single outline
coo <- matrix(runif(100), ncol = 2)
coefs <- eft(coo, nb_h = 6)

# Normalize
coefs_norm <- eft_norm(coefs)

# Preserve starting point
coefs_norm_start <- eft_norm(coefs, start = TRUE)

# Compare first harmonic before/after
coefs[1:4]
#>          A1          A2          A3          A4 
#> -0.07188682 -0.02530000  0.04252666 -0.03175744 
coefs_norm[1:4]
#>         A1         A2         A3         A4 
#>  1.0000000  1.1182033 -0.5510850 -0.1854224 

if (FALSE) { # \dontrun{
# Typical workflow with tibble
library(dplyr)
bot %>%
  eft(nb_h = 8) %>%
  eft_norm()  # Normalizes in place

# Create new column instead
bot %>%
  eft(nb_h = 8) %>%
  eft_norm(.name = "coe_norm")
} # }
```
