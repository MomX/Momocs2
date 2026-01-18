# npoly ----

#' Natural Polynomial Transform
#'
#' Compute natural polynomial coefficients from open curve coordinates.
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
#'   with classes `c("npoly", "numeric")` containing degree+1 coefficients (x0, x1, ..., xN)
#' * If `x` is a single matrix and `raw = TRUE`: returns a list with elements
#'   `coeff`, `degree`, `baseline1`, `baseline2`, `r2`, `mod`
#' * If `x` is a list: returns a list of coefficient vectors (each with class `c("npoly", "numeric")`)
#' * If `x` is a tibble: returns the tibble with new list-column(s) of class
#'   `c("npoly", "coe", "list")` containing coefficient vectors
#'
#' @details
#' Natural polynomial fitting uses standard power basis polynomials (1, x, x², x³, ...)
#' to decompose open curves. The method fits a polynomial model to the y-coordinates
#' as a function of x-coordinates, after baseline normalization.
#'
#' Unlike orthogonal polynomials, natural polynomials have coefficients that change
#' when higher degrees are added. However, they are more interpretable and directly
#' correspond to the polynomial equation.
#'
#' The curve is automatically baseline-normalized using the first and last points
#' as landmarks, positioned at (-0.5, 0) and (0.5, 0) respectively.
#'
#' ## Choosing the polynomial degree
#'
#' The default `degree = 5` works well for most open curves. Higher degrees capture
#' more detail but increase dimensionality and risk overfitting. Use `raw = TRUE`
#' to examine the R² value and assess fit quality.
#'
#' @examples
#' # Single curve
#' data(olea)
#' cur <- olea$VD[[1]]
#' npoly_coefs <- npoly(cur)
#' npoly_coefs
#'
#' # Get raw output with R² and model details
#' npoly(cur, raw = TRUE)
#'
#' # With higher degree
#' npoly(cur, degree = 8)
#'
#' # List of curves
#' cur_list <- list(cur, cur * 1.5, cur * 2)
#' npoly(cur_list)
#'
#' \dontrun{
#' # Tibble - processes all coo columns by default
#' library(dplyr)
#' olea %>%
#'   npoly(degree = 6)  # Creates VD_coe and VL_coe
#'
#' # Process specific column only
#' olea %>%
#'   npoly(.cols = VD)
#'
#' # Custom name for single column
#' olea %>%
#'   npoly(.cols = VL, .name = "custom_name")
#' }
#'
#' @seealso [npoly_i()] for inverse transform, [opoly()] for orthogonal polynomials,
#'   [dct()] for discrete cosine transform.
#'
#' @export
npoly <- function(x, degree = 5, raw = FALSE, ..., .cols = NULL, .name = "_coe") {

  # Single matrix case
  if (is.matrix(x)) {
    return(.npoly(x, degree = degree, raw = raw))
  }

  # List case
  if (is.list(x) && !is.data.frame(x)) {
    result <- lapply(x, .npoly, degree = degree, raw = FALSE)
    class(result) <- c("npoly", "coe", "list")
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
      result_list <- lapply(x[[col]], .npoly, degree = degree, raw = FALSE)

      # Add appropriate classes
      class(result_list) <- c("npoly", "coe", "list")

      # Add to tibble
      x[[out_col]] <- result_list
    }

    return(x)
  }

  stop("x must be a matrix, list, or tibble")
}


# .npoly (internal worker) ----

#' @keywords internal
.npoly <- function(x, degree = 5, raw = FALSE) {
  # Apply baseline normalization
  coo <- coo_baseline(x, id1 = 1, id2 = nrow(x))

  # Store baseline points for raw output
  baseline1 <- coo[1, ]
  baseline2 <- coo[nrow(coo), ]

  # Fit natural polynomial model (raw = TRUE for power basis)
  x_poly <- poly(coo[, 1], degree = degree, raw = TRUE)
  mod <- lm(coo[, 2] ~ x_poly)
  r2 <- summary(mod)$r.squared

  # Get and name coefficients
  coefs <- mod$coefficients
  coefs <- npoly_name(coefs)

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
    class(coefs) <- c("npoly", "numeric")
    coefs
  }
}


# npoly_i ----

#' Inverse natural polynomial transform
#'
#' Reconstruct curve coordinates from natural polynomial coefficients.
#'
#' @param x A numeric vector, list (from raw npoly), list of vectors, or
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
#' Performs inverse natural polynomial transform to reconstruct curve coordinates
#' from coefficients. The curve is reconstructed between baseline points at
#' (-0.5, 0) and (0.5, 0), then re-registered to original baseline.
#'
#' @examples
#' # Single curve
#' data(olea)
#' cur <- olea$VD[[1]]
#' coefs <- npoly(cur, degree = 6)
#'
#' # Reconstruct
#' cur_reconstructed <- npoly_i(coefs)
#'
#' # From raw output
#' coefs_raw <- npoly(cur, raw = TRUE)
#' npoly_i(coefs_raw)
#'
#' \dontrun{
#' # Tibble workflow
#' library(dplyr)
#' olea %>%
#'   npoly(.cols = VD) %>%
#'   npoly_i()  # Creates VD_coe_i
#'
#' # Custom column names
#' olea %>%
#'   npoly(.cols = VD) %>%
#'   npoly_i(.cols = VD_coe, .name = "VD_reconstructed")
#' }
#'
#' @seealso [npoly()] for forward transform
#'
#' @export
npoly_i <- function(x, nb_pts = 120, ..., .cols = NULL, .name = "_i") {

  # Single vector or raw list case
  if (is.numeric(x) || (is.list(x) && !is.data.frame(x) &&
                        all(c("coeff", "degree") %in% names(x)))) {
    return(.npoly_i(x, nb_pts = nb_pts))
  }

  # List of vectors case
  if (is.list(x) && !is.data.frame(x)) {
    result <- lapply(x, .npoly_i, nb_pts = nb_pts)
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
      result_list <- lapply(x[[col]], .npoly_i, nb_pts = nb_pts)

      # Add appropriate classes
      class(result_list) <- c("opn", "coo", "list")

      # Add to tibble
      x[[out_col]] <- result_list
    }

    return(x)
  }

  stop("x must be a vector, list, or tibble")
}


# .npoly_i (internal worker) ----

#' @keywords internal
.npoly_i <- function(x, nb_pts = 120) {
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

  # Compute y using natural polynomial formula: y = c0 + c1*x + c2*x^2 + ...
  y_new <- coeff[1]  # Intercept
  for (i in 1:degree) {
    y_new <- y_new + (x_new^i * coeff[i + 1])
  }

  # Combine into matrix
  coo <- cbind(x_new, y_new)

  # Re-register to original baseline (inverse of normalization)
  coo <- coo_baseline(coo, 1, nrow(coo), t1 = baseline1, t2 = baseline2)

  # Set column names and class
  colnames(coo) <- c("x", "y")
  class(coo) <- c("xy", "matrix")
  coo
}


# npoly_name ----

#' Name natural polynomial coefficients
#'
#' Add standard names to an npoly coefficient vector.
#'
#' @param x Numeric vector of npoly coefficients (length = degree + 1).
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
#' npoly_name(coefs)
#'
#' @export
npoly_name <- function(x) {
  degree <- length(x) - 1
  names(x) <- paste0("x", 0:degree)
  x
}
