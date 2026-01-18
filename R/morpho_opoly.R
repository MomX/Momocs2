# opoly ----

#' Orthogonal Polynomial Transform
#'
#' Compute orthogonal (Legendre) polynomial coefficients from open curve coordinates.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param degree Integer. Polynomial degree for the fit. Default is 5.
#' @param raw Logical. If `TRUE`, returns a list with coefficients and fit details
#'   (only for single matrix). Default is `FALSE`.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   processes all coo columns automatically.
#' @param .name Character. Name suffix for output coefficient columns when `x` is
#'   a tibble. Default is `"_coe"`. Output columns will be named `originalname_coe`.
#'   If a single custom name is provided and multiple columns are processed, an error
#'   is raised.
#'
#' @return
#' * If `x` is a single matrix and `raw = FALSE`: returns a named numeric vector
#'   with classes `c("opoly", "numeric")` containing degree+1 coefficients (x0, x1, ..., xN)
#' * If `x` is a single matrix and `raw = TRUE`: returns a list with elements
#'   `coeff`, `degree`, `baseline1`, `baseline2`, `r2`, `mod`
#' * If `x` is a list: returns a list of coefficient vectors (each with class `c("opoly", "numeric")`)
#' * If `x` is a tibble: returns the tibble with new list-column(s) of class
#'   `c("opoly", "coe", "list")` containing coefficient vectors
#'
#' @details
#' Orthogonal polynomial fitting uses Legendre polynomials to decompose open curves.
#' The method fits a polynomial model to the y-coordinates as a function of x-coordinates,
#' after baseline normalization.
#'
#' Orthogonal polynomials are preferred over natural polynomials because adding a
#' higher degree does not change the lower-order coefficients. This makes them more
#' stable for comparative analysis.
#'
#' The curve is automatically baseline-normalized using the first and last points
#' as landmarks, positioned at (-0.5, 0) and (0.5, 0) respectively.
#'
#' ## Choosing the polynomial degree
#'
#' The default `degree = 5` works well for most open curves. Higher degrees capture
#' more detail but increase dimensionality. Use `raw = TRUE` to examine the R²
#' value and assess fit quality.
#'
#' @examples
#' # Single curve
#' data(olea)
#' cur <- olea$VD[[1]]
#' opoly_coefs <- opoly(cur)
#' opoly_coefs
#'
#' # Get raw output with R² and model details
#' opoly(cur, raw = TRUE)
#'
#' # With higher degree
#' opoly(cur, degree = 8)
#'
#' # List of curves
#' cur_list <- list(cur, cur * 1.5, cur * 2)
#' opoly(cur_list)
#'
#' \dontrun{
#' # Tibble - processes all coo columns by default
#' olea %>%
#'   opoly(degree = 6)  # Creates VD_coe and VL_coe
#'
#' # Process specific column only
#' olea %>%
#'   opoly(.cols = VD)
#'
#' # Custom name for single column
#' olea %>%
#'   opoly(.cols = VL, .name = "custom_name")
#' }
#'
#' @seealso [opoly_i()] for inverse transform, [npoly()] for natural polynomials,
#'   [dct()] for discrete cosine transform.
#'
#' @export
opoly <- function(x, degree = 5, raw = FALSE, ..., .cols = NULL, .name = "_coe") {

  # Single matrix case
  if (is.matrix(x)) {
    return(.opoly(x, degree = degree, raw = raw))
  }

  # List case
  if (is.list(x) && !is.data.frame(x)) {
    result <- lapply(x, .opoly, degree = degree, raw = FALSE)
    class(result) <- c("opoly", "coe", "list")
    return(result)
  }

  # Tibble case
  if (is.data.frame(x)) {
    # Capture .cols with tidyeval
    .cols_quo <- rlang::enquo(.cols)

    if (rlang::quo_is_null(.cols_quo)) {
      # Process ALL coo columns
      cols_to_process <- get_all_coo_cols(x)
      message(sprintf("Processing coo column(s): %s", paste(cols_to_process, collapse = ", ")))
    } else {
      # User specified columns
      cols_idx <- tidyselect::eval_select(.cols_quo, x)
      cols_to_process <- names(cols_idx)
    }

    # Message about degree if default is used
    if (degree == 5) {
      message(sprintf("Using degree = %d (default)", degree))
    }

    # Determine output column names
    if (.name == "_coe") {
      # Default: append _coe to original column names
      output_cols <- paste0(cols_to_process, "_coe")
    } else {
      # Custom name provided
      if (length(cols_to_process) > 1) {
        stop("Cannot use custom .name when processing multiple columns. Use default '_coe' suffix or specify single column with .cols.")
      }
      output_cols <- .name
    }

    # Check if output columns already exist
    existing <- intersect(output_cols, names(x))
    if (length(existing) > 0) {
      stop(sprintf("Column(s) already exist: %s. Choose different name(s).",
                   paste(existing, collapse = ", ")))
    }

    # Process each column
    for (i in seq_along(cols_to_process)) {
      col <- cols_to_process[i]
      out_col <- output_cols[i]

      # Compute coefficients for each row
      result_list <- lapply(x[[col]], .opoly, degree = degree, raw = FALSE)

      # Add appropriate classes
      class(result_list) <- c("opoly", "coe", "list")

      # Add to tibble
      x[[out_col]] <- result_list
    }

    return(x)
  }

  stop("x must be a matrix, list, or tibble")
}


# .opoly (internal worker) ----

#' @keywords internal
.opoly <- function(x, degree = 5, raw = FALSE) {
  # Apply baseline normalization
  coo <- coo_baseline(x, id1 = 1, id2 = nrow(x))

  # Store baseline points for raw output
  baseline1 <- coo[1, ]
  baseline2 <- coo[nrow(coo), ]

  # Fit orthogonal polynomial model
  x_poly <- poly(coo[, 1], degree = degree, raw = FALSE)
  mod <- lm(coo[, 2] ~ x_poly)
  r2 <- summary(mod)$r.squared

  # Get and name coefficients
  coefs <- mod$coefficients
  coefs <- opoly_name(coefs)

  # Return format depends on raw argument
  if (raw) {
    list(
      coeff = coefs,  # Now named!
      degree = degree,
      baseline1 = baseline1,
      baseline2 = baseline2,
      r2 = r2,
      mod = mod
    )
  } else {
    class(coefs) <- c("opoly", "numeric")
    coefs
  }
}


# opoly_i ----

#' Inverse orthogonal polynomial transform
#'
#' Reconstruct curve coordinates from orthogonal polynomial coefficients.
#'
#' @param x A numeric vector, list (from raw opoly), list of vectors, or
#'   tibble with coe columns.
#' @param nb_pts Integer. Number of points to reconstruct. Default is 120.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   processes all coe columns automatically.
#' @param .name Character. Name suffix for output curve columns when `x` is a tibble.
#'   Default is `"_i"`. Output columns will be named `originalname_i`.
#'
#' @return
#' * If `x` is a vector or raw list: returns a matrix (nb_pts x 2) with class `"xy"`
#' * If `x` is a list of vectors: returns a list of matrices
#' * If `x` is a tibble: returns the tibble with new cur column(s) added
#'
#' @details
#' Performs inverse orthogonal polynomial transform to reconstruct curve coordinates
#' from coefficients. The curve is reconstructed between baseline points at
#' (-0.5, 0) and (0.5, 0), then re-registered to original baseline.
#'
#' @examples
#' # Single curve
#' data(olea)
#' cur <- olea$VD[[1]]
#' coefs <- opoly(cur, degree = 6)
#'
#' # Reconstruct
#' cur_reconstructed <- opoly_i(coefs)
#'
#' # From raw output
#' coefs_raw <- opoly(cur, raw = TRUE)
#' opoly_i(coefs_raw)
#'
#' \dontrun{
#' # Tibble workflow
#' library(dplyr)
#' olea %>%
#'   opoly(.cols = VD) %>%
#'   opoly_i()  # Creates VD_coe_i
#'
#' # Custom column names
#' olea %>%
#'   opoly(.cols = VD) %>%
#'   opoly_i(.cols = VD_coe, .name = "VD_reconstructed")
#' }
#'
#' @seealso [opoly()] for forward transform
#'
#' @export
opoly_i <- function(x, nb_pts = 120, ..., .cols = NULL, .name = "_i") {

  # Single vector or raw list case
  if (is.numeric(x) || (is.list(x) && !is.data.frame(x) &&
                        all(c("coeff", "degree") %in% names(x)))) {
    return(.opoly_i(x, nb_pts = nb_pts))
  }

  # List of vectors case
  if (is.list(x) && !is.data.frame(x)) {
    result <- lapply(x, .opoly_i, nb_pts = nb_pts)
    class(result) <- c("opn", "coo", "list")
    return(result)
  }

  # Tibble case
  if (is.data.frame(x)) {
    # Capture .cols with tidyeval
    .cols_quo <- rlang::enquo(.cols)

    if (rlang::quo_is_null(.cols_quo)) {
      # Process ALL coe columns
      cols_to_process <- get_all_coe_cols(x)
      message(sprintf("Processing coe column(s): %s", paste(cols_to_process, collapse = ", ")))
    } else {
      # User specified columns
      cols_idx <- tidyselect::eval_select(.cols_quo, x)
      cols_to_process <- names(cols_idx)
    }

    # Determine output column names
    if (.name == "_i") {
      # Default: append _i to original column names
      output_cols <- paste0(cols_to_process, "_i")
    } else {
      # Custom name provided
      if (length(cols_to_process) > 1) {
        stop("Cannot use custom .name when processing multiple columns. Use default '_i' suffix or specify single column with .cols.")
      }
      output_cols <- .name
    }

    # Check if output columns already exist
    existing <- intersect(output_cols, names(x))
    if (length(existing) > 0) {
      stop(sprintf("Column(s) already exist: %s. Choose different name(s).",
                   paste(existing, collapse = ", ")))
    }

    # Process each column
    for (i in seq_along(cols_to_process)) {
      col <- cols_to_process[i]
      out_col <- output_cols[i]

      # Reconstruct for each row
      result_list <- lapply(x[[col]], .opoly_i, nb_pts = nb_pts)

      # Add appropriate classes
      class(result_list) <- c("opn", "coo", "list")

      # Add to tibble
      x[[out_col]] <- result_list
    }

    return(x)
  }

  stop("x must be a vector, list, or tibble")
}


# .opoly_i (internal worker) ----

#' @keywords internal
.opoly_i <- function(x, nb_pts = 120) {
  # Helper function for matrix multiplication with scalar vector
  .mprod <- function(m, s) {
    res <- m
    for (i in 1:ncol(m)) {
      res[, i] <- m[, i] * s[i]
    }
    return(res)
  }

  # If vector, extract components
  if (is.numeric(x)) {
    coeff <- x
    degree <- length(coeff) - 1
    baseline1 <- c(-0.5, 0)
    baseline2 <- c(0.5, 0)
  } else {
    # It's a raw list
    coeff <- x$coeff
    degree <- x$degree
    baseline1 <- x$baseline1
    baseline2 <- x$baseline2
  }

  # Generate x coordinates along baseline
  x_new <- seq(baseline1[1], baseline2[1], length = nb_pts)

  # Generate orthogonal polynomial basis
  x_poly <- poly(x_new, degree = degree)

  # Predict y coordinates
  y_pred <- predict(x_poly, x_new)
  y_new <- coeff[1] + apply(.mprod(m = y_pred, s = coeff[-1]), 1, sum)

  # Combine into matrix
  coo <- cbind(x_new, y_new)

  # Re-register to original baseline (inverse of normalization)
  coo <- coo_baseline(coo, 1, nrow(coo), t1 = baseline1, t2 = baseline2)

  # Set column names and class
  colnames(coo) <- c("x", "y")
  class(coo) <- c("xy", "matrix")
  coo
}


# opoly_name ----

#' Name orthogonal polynomial coefficients
#'
#' Add standard names to an opoly coefficient vector.
#'
#' @param x Numeric vector of opoly coefficients (length = degree + 1).
#'
#' @return Named numeric vector with names in the format x0, x1, x2, ..., xN
#'   where x0 is the intercept.
#'
#' @details
#' Names are assigned as x0 (intercept), x1, x2, ..., xN where N is the
#' polynomial degree.
#'
#' @examples
#' # Create unnamed coefficient vector
#' coefs <- runif(6)  # degree 5
#' opoly_name(coefs)
#'
#' @export
opoly_name <- function(x) {
  degree <- length(x) - 1
  names(x) <- paste0("x", 0:degree)
  x
}
