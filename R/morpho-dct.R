# dct ----

#' Discrete Cosine Transform
#'
#' Compute discrete cosine transform coefficients from open curve coordinates.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param nb_h Integer. Number of harmonics to compute. Default is 12.
#' @param raw Logical. If `TRUE`, returns a list with A, B coefficients and
#'   additional components (only for single matrix). Default is `FALSE`.
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
#'   with classes `c("dct", "numeric")` containing 2*nb_h coefficients (A1-An, B1-Bn)
#' * If `x` is a single matrix and `raw = TRUE`: returns a list with elements
#'   `an`, `bn`, `mod`, `arg`
#' * If `x` is a list: returns a list of coefficient vectors (each with class `c("dct", "numeric")`)
#' * If `x` is a tibble: returns the tibble with new list-column(s) of class
#'   `c("dct", "coe", "list")` containing coefficient vectors
#'
#' @details
#' The Discrete Cosine Transform (DCT) is a Fourier-related method for analyzing
#' open contours. It decomposes curves into a sum of cosine functions with different
#' frequencies, capturing shape variation efficiently.
#'
#' For each harmonic n, two coefficients are computed:
#' * An: Real part (related to x-coordinates)
#' * Bn: Imaginary part (related to y-coordinates)
#'
#' The method also computes modulus and argument for each harmonic when `raw = TRUE`:
#' * mod: Amplitude of each harmonic
#' * arg: Phase angle of each harmonic
#'
#' The curve is automatically baseline-normalized using the first and last points
#' as landmarks, positioned at (-0.5, 0) and (0.5, 0) respectively.
#'
#' ## Choosing the number of harmonics
#'
#' The default `nb_h = 12` works well for most open curves. More harmonics capture
#' finer details but increase dimensionality. Unlike polynomial methods, DCT
#' harmonics are independent and don't change when adding more.
#'
#' @note This method can be computationally intensive for large datasets. The
#' implementation is optimized but may still take time for many shapes.
#'
#' @references
#' Dommergues, C. H., Dommergues, J.-L., & Verrecchia, E. P. (2007).
#' The Discrete Cosine Transform, a Fourier-related Method for Morphometric
#' Analysis of Open Contours. \emph{Mathematical Geology}, 39(8), 749-763.
#'
#' @examples
#' # Single curve
#' data(olea)
#' cur <- olea$VD[[1]]
#' dct_coefs <- dct(cur)
#' dct_coefs
#'
#' # Get raw output with modulus and argument
#' dct(cur, raw = TRUE)
#'
#' # With more harmonics
#' dct(cur, nb_h = 15)
#'
#' # List of curves
#' cur_list <- list(cur, cur * 1.5, cur * 2)
#' dct(cur_list)
#'
#' \dontrun{
#' # Tibble - processes all coo columns by default
#' library(dplyr)
#' olea %>%
#'   dct(nb_h = 10)  # Creates VD_coe and VL_coe
#'
#' # Process specific column only
#' olea %>%
#'   dct(.cols = VD)
#'
#' # Custom name for single column
#' olea %>%
#'   dct(.cols = VL, .name = "dct_coeffs")
#' }
#'
#' @seealso [dct_i()] for inverse transform, [opoly()], [npoly()] for polynomial methods.
#'
#' @export
dct <- function(x, nb_h = 12, raw = FALSE, ..., .cols = NULL, .name = "_coe") {

  # Single matrix case
  if (is.matrix(x)) {
    return(.dct(x, nb_h = nb_h, raw = raw))
  }

  # List case
  if (is.list(x) && !is.data.frame(x)) {
    result <- lapply(x, .dct, nb_h = nb_h, raw = FALSE)
    class(result) <- c("dct", "coe", "list")
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

    # Message about nb_h if default is used
    if (nb_h == 12) {
      message(sprintf("Using nb_h = %d harmonics (default)", nb_h))
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
      result_list <- lapply(x[[col]], .dct, nb_h = nb_h, raw = FALSE)

      # Add appropriate classes
      class(result_list) <- c("dct", "coe", "list")

      # Add to tibble
      x[[out_col]] <- result_list
    }

    return(x)
  }

  stop("x must be a matrix, list, or tibble")
}


# .dct (internal worker) ----

#' @keywords internal
.dct <- function(x, nb_h = 12, raw = FALSE) {
  # Apply baseline normalization
  coo <- coo_baseline(x, id1 = 1, id2 = nrow(x))

  # Store baseline points for raw output
  baseline1 <- coo[1, ]
  baseline2 <- coo[nrow(coo), ]

  # DCT algorithm (adapted from Momocs dfourier)
  N <- nrow(coo)
  pol <- coo[, 1] + (0+1i) * coo[, 2]

  # Prepare normalization constants
  c <- rep(sqrt(2/N), N)
  c[1] <- 1/sqrt(N)

  # Compute DCT coefficients
  Sv <- S <- rep(NA, N)
  for (k in 0:(N-1)) {
    Sv <- pol * cos(((2 * 0:(N-1)) * k * pi)/(2 * N))
    S[k+1] <- c[k+1] * sum(Sv)
  }

  # Remove first harmonic (trivial) and keep only nb_h harmonics
  S <- S[2:(nb_h+1)]

  # Extract components
  an <- Re(S)
  bn <- Im(S)
  mod <- Mod(S)
  arg <- Arg(S)

  # Return format depends on raw argument
  if (raw) {
    list(
      an = an,
      bn = bn,
      mod = mod,
      arg = arg,
      baseline1 = baseline1,
      baseline2 = baseline2
    )
  } else {
    # Combine into single vector and name
    coefs <- c(an, bn)
    coefs <- dct_name(coefs)
    class(coefs) <- c("dct", "numeric")
    coefs
  }
}


# dct_i ----

#' Inverse discrete cosine transform
#'
#' Reconstruct curve coordinates from discrete cosine transform coefficients.
#'
#' @param x A numeric vector, list (from raw dct), list of vectors, or
#'   tibble with coe columns.
#' @param nb_h Integer. Number of harmonics to use for reconstruction. If `NULL`,
#'   uses all available harmonics.
#' @param nb_pts Integer. Number of points to reconstruct. Default is 60.
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
#' Performs inverse discrete cosine transform to reconstruct curve coordinates
#' from coefficients. This is useful for:
#' * Visualizing shapes at different harmonic levels
#' * Reconstructing shapes from PCA or other statistical analyses
#' * Filtering out high-frequency noise by using fewer harmonics
#'
#' If `nb_h` is less than the total number of harmonics in the coefficients,
#' only the first `nb_h` harmonics are used (low-pass filtering).
#'
#' @examples
#' # Single curve
#' data(olea)
#' cur <- olea$VD[[1]]
#' coefs <- dct(cur, nb_h = 12)
#'
#' # Reconstruct with all harmonics
#' cur_reconstructed <- dct_i(coefs)
#'
#' # Reconstruct with fewer harmonics (smoothing)
#' cur_smooth <- dct_i(coefs, nb_h = 6)
#'
#' # From raw output
#' coefs_raw <- dct(cur, raw = TRUE)
#' dct_i(coefs_raw)
#'
#' \dontrun{
#' # Tibble workflow
#' library(dplyr)
#' olea %>%
#'   dct(.cols = VD) %>%
#'   dct_i()  # Creates VD_coe_i
#'
#' # Custom column names
#' olea %>%
#'   dct(.cols = VD) %>%
#'   dct_i(.cols = VD_coe, .name = "VD_reconstructed")
#' }
#'
#' @seealso [dct()] for forward transform
#'
#' @export
dct_i <- function(x, nb_h = NULL, nb_pts = 60, ..., .cols = NULL, .name = "_i") {

  # Single vector or raw list case
  if (is.numeric(x) || (is.list(x) && !is.data.frame(x) &&
                        all(c("an", "bn") %in% names(x)))) {
    return(.dct_i(x, nb_h = nb_h, nb_pts = nb_pts))
  }

  # List of vectors case
  if (is.list(x) && !is.data.frame(x)) {
    result <- lapply(x, .dct_i, nb_h = nb_h, nb_pts = nb_pts)
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
      result_list <- lapply(x[[col]], .dct_i, nb_h = nb_h, nb_pts = nb_pts)

      # Add appropriate classes
      class(result_list) <- c("opn", "coo", "list")

      # Add to tibble
      x[[out_col]] <- result_list
    }

    return(x)
  }

  stop("x must be a vector, list, or tibble")
}


# .dct_i (internal worker) ----

#' @keywords internal
.dct_i <- function(x, nb_h = NULL, nb_pts = 60) {
  # If vector, split into components
  if (is.numeric(x)) {
    x <- dct_split(x)
    baseline1 <- c(-0.5, 0)
    baseline2 <- c(0.5, 0)
  } else {
    # It's a raw list
    baseline1 <- x$baseline1
    baseline2 <- x$baseline2
  }

  # Extract A and B coefficients
  A <- x$an
  B <- x$bn

  # Determine number of harmonics to use
  if (is.null(nb_h)) {
    nb_h <- length(A) + 1  # +1 because we removed first harmonic in forward transform
  } else {
    # Truncate to requested harmonics
    nb_h <- min(nb_h, length(A) + 1)
    if (nb_h > 1) {
      A <- A[1:(nb_h-1)]
      B <- B[1:(nb_h-1)]
    }
  }

  # Prepare normalization constants
  c <- rep(sqrt(2/nb_pts), nb_pts)
  c[1] <- 1/sqrt(nb_pts)

  # Reconstruct complex coefficients
  S <- A + (0+1i) * B
  S <- c(0+1i, S)  # Add trivial first harmonic (0, 0)

  # Inverse DCT
  sv_r <- rep(NA, nb_h)
  s_r <- rep(NA, nb_pts)

  for (n in 0:(nb_pts - 1)) {
    for (k in 0:(nb_h - 1)) {
      sv_r[k + 1] <- c[k + 1] * S[k + 1] * cos(((2 * n + 1) * k * pi)/(2 * nb_pts))
    }
    s_r[n + 1] <- sum(sv_r)
  }

  # Extract x and y coordinates
  coo <- cbind(Re(s_r), Im(s_r))

  # Re-register to original baseline (inverse of normalization)
  coo <- coo_baseline(coo, 1, nrow(coo), t1 = baseline1, t2 = baseline2)

  # Set column names and class
  colnames(coo) <- c("x", "y")
  class(coo) <- c("xy", "matrix")
  coo
}


# dct_name ----

#' Name discrete cosine transform coefficients
#'
#' Add standard names to a DCT coefficient vector.
#'
#' @param x Numeric vector of DCT coefficients (length must be even: 2*nb_h).
#'
#' @return Named numeric vector with names in the format A1, A2, ..., An,
#'   B1, B2, ..., Bn where n is the number of harmonics.
#'
#' @details
#' Names are assigned in the order: A1-An (real parts), B1-Bn (imaginary parts),
#' where n is the number of harmonics.
#'
#' @examples
#' # Create unnamed coefficient vector
#' coefs <- runif(24)  # 12 harmonics
#' dct_name(coefs)
#'
#' @export
dct_name <- function(x) {
  n <- length(x) / 2
  names(x) <- c(
    paste0("A", 1:n),  # A coefficients
    paste0("B", 1:n)   # B coefficients
  )
  x
}


# dct_split ----

#' Split DCT coefficient vector into components
#'
#' Separate a flattened DCT coefficient vector into its An and Bn components.
#'
#' @param x Numeric vector of DCT coefficients (length must be even).
#'
#' @return List with two elements:
#' * `an`: An coefficients (real parts)
#' * `bn`: Bn coefficients (imaginary parts)
#'
#' @examples
#' # Create coefficient vector
#' data(olea)
#' coefs <- dct(olea$VD[[1]], nb_h = 12)
#' dct_split(coefs)
#'
#' @seealso [dct()] to generate coefficients
#'
#' @export
dct_split <- function(x) {
  # Number of harmonics
  nb_h <- length(x) / 2

  # Split into A and B components
  list(
    an = x[1:nb_h],
    bn = x[(nb_h + 1):(2 * nb_h)]
  )
}
