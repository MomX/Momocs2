# efourier ----

#' Elliptic Fourier Transform
#'
#' Compute elliptic Fourier transform coefficients from outline coordinates.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param nb_h Integer. Number of harmonics to compute. Default is 6.
#' @param raw Logical. If `TRUE`, returns a list with a0, c0 and harmonic
#'   coefficients (only for single matrix). Default is `FALSE`.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects. If multiple coo
#'   columns exist, must be specified explicitly.
#' @param .name Character. Name for the output coefficient column when `x` is
#'   a tibble. Default is `"coe"`. If this column already exists, an error is raised.
#'
#' @return
#' * If `x` is a single matrix and `raw = FALSE`: returns a named numeric vector
#'   with classes `c("eft", "numeric")` containing 4*nb_h coefficients (An, Bn, Cn, Dn)
#' * If `x` is a single matrix and `raw = TRUE`: returns a list with elements
#'   `an`, `bn`, `cn`, `dn`, `a0`, `c0`
#' * If `x` is a list: returns a list of coefficient vectors (each with class `c("eft", "numeric")`)
#' * If `x` is a tibble: returns the tibble with a new list-column of class
#'   `c("eft", "coe", "list")` containing coefficient vectors
#'
#' @details
#' Elliptic Fourier analysis decomposes a closed outline into a sum of harmonically
#' related ellipses. The method is based on separate Fourier decompositions of the
#' incremental changes in x and y coordinates as a function of cumulative perimeter
#' distance.
#'
#' For each harmonic n, four coefficients are computed:
#' * An, Bn: coefficients for x-coordinates (cosine and sine terms)
#' * Cn, Dn: coefficients for y-coordinates (cosine and sine terms)
#'
#' The `raw = TRUE` option additionally returns `a0` and `c0` (the constant terms),
#' which represent the centroid position. This is only available for single matrices.
#'
#' ## Choosing the number of harmonics
#'
#' The default `nb_h = 6` works well for most outlines. More harmonics capture finer
#' details but increase dimensionality. Use fewer harmonics for simpler shapes or
#' to reduce noise. As a rule of thumb, nb_h should be less than the number of
#' outline points divided by 3.
#'
#' @examples
#' # Single outline
#' coo <- matrix(runif(100), ncol = 2)
#' eft_coefs <- efourier(coo)
#' eft_coefs
#'
#' # Get raw output with a0 and c0
#' efourier(coo, raw = TRUE)
#'
#' # With more harmonics
#' efourier(coo, nb_h = 10)
#'
#' # List of outlines
#' coo_list <- list(coo, coo * 1.5, coo * 2)
#' efourier(coo_list)
#'
#' \dontrun{
#' # Tibble (requires coo column)
#' library(dplyr)
#' bot %>%
#'   efourier(nb_h = 8)
#'
#' # Specify column explicitly
#' bot %>%
#'   efourier(.cols = coo, .name = "eft_coefs")
#' }
#'
#' @seealso [efourier_i()] for inverse transform, [efourier_norm()] for numeric normalisation.
#'
#' @export
efourier <- function(x, nb_h = 6, raw = FALSE, ..., .cols = NULL, .name = "coe") {

  # Single matrix case
  if (is.matrix(x)) {
    return(.efourier(x, nb_h = nb_h, raw = raw))
  }

  # List case
  if (is.list(x) && !is.data.frame(x)) {
    result <- lapply(x, .efourier, nb_h = nb_h, raw = FALSE)
    class(result) <- c("eft", "coe", "list")
    return(result)
  }

  # Tibble case
  if (is.data.frame(x)) {
    # Check if output column already exists
    if (.name %in% names(x)) {
      stop(sprintf("Column '%s' already exists. Choose a different name with .name argument.", .name))
    }

    # Capture .cols with tidyeval
    .cols_quo <- rlang::enquo(.cols)
    if (rlang::quo_is_null(.cols_quo)) {
      # Try to auto-detect
      tryCatch({
        cols_to_process <- get_coo_cols(x, NULL)
        message(sprintf("Using coo column: '%s'", cols_to_process))
      }, error = function(e) {
        stop("Could not auto-detect coo column. Please specify with .cols argument.\n",
             "Original error: ", e$message)
      })
    } else {
      cols_idx <- tidyselect::eval_select(.cols_quo, x)
      cols_to_process <- names(cols_idx)
    }

    # Process first (or only) coo column
    col <- cols_to_process[1]

    # Message about nb_h if default is used
    if (nb_h == 6) {
      message(sprintf("Using nb_h = %d harmonics (default)", nb_h))
    }

    # Compute coefficients for each row
    result_list <- lapply(x[[col]], .efourier, nb_h = nb_h, raw = FALSE)

    # Add appropriate classes
    class(result_list) <- c("eft", "coe", "list")

    # Add to tibble
    x[[.name]] <- result_list

    return(x)
  }

  stop("x must be a matrix, list, or tibble")
}


# .efourier (internal worker) ----

#' @keywords internal
.efourier <- function(x, nb_h = 6, raw = FALSE) {
  coo <- x
  nr <- nrow(x)

  # Calculate incremental changes
  dx <- coo[, 1] - coo[, 1][c(nr, (1:(nr - 1)))]
  dy <- coo[, 2] - coo[, 2][c(nr, (1:(nr - 1)))]
  dt <- sqrt(dx^2 + dy^2)
  dt[dt < 1e-10] <- 1e-10

  # Cumulative distance
  t1 <- cumsum(dt)
  t1m1 <- c(0, t1[-nr])
  T <- sum(dt)

  # Prepare to host results
  an <- bn <- cn <- dn <- numeric(nb_h)

  # Core EFT algorithm
  for (i in 1:nb_h) {
    Ti <- (T / (2 * pi^2 * i^2))
    r <- 2 * i * pi
    an[i] <- Ti * sum((dx/dt) * (cos(r * t1/T) - cos(r * t1m1/T)))
    bn[i] <- Ti * sum((dx/dt) * (sin(r * t1/T) - sin(r * t1m1/T)))
    cn[i] <- Ti * sum((dy/dt) * (cos(r * t1/T) - cos(r * t1m1/T)))
    dn[i] <- Ti * sum((dy/dt) * (sin(r * t1/T) - sin(r * t1m1/T)))
  }

  # Constant terms (centroid)
  a0 <- 2 * sum(coo[, 1] * dt/T)
  c0 <- 2 * sum(coo[, 2] * dt/T)

  # Return format depends on raw argument
  if (raw) {
    list(an = an, bn = bn, cn = cn, dn = dn, a0 = a0, c0 = c0)
  } else {
    coefs <- c(an, bn, cn, dn)
    coefs <- efourier_name(coefs)
    class(coefs) <- c("eft", "numeric")
    coefs
  }
}

# efourier_i ----

#' Inverse elliptic Fourier transform
#'
#' Reconstruct outline coordinates from elliptic Fourier coefficients.
#'
#' @param x A numeric vector, list (from raw efourier), list of vectors, or
#'   tibble with coe columns.
#' @param nb_h Integer. Number of harmonics to use for reconstruction. If `NULL`,
#'   uses all available harmonics.
#' @param nb_pts Integer. Number of points to reconstruct. Default is 120.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coe objects.
#' @param .name Character. Name for the output coo column when `x` is a tibble.
#'   If `NULL`, uses the pattern `"colname_i"` (e.g., `"coe_i"`).
#'
#' @return
#' * If `x` is a vector or raw list: returns a matrix (nb_pts x 2) with class `"xy"`
#' * If `x` is a list of vectors: returns a list of matrices
#' * If `x` is a tibble: returns the tibble with a new coo column added
#'
#' @details
#' Performs inverse elliptic Fourier transform to reconstruct outline coordinates
#' from coefficients. This is useful for:
#' * Visualizing shapes at different harmonic levels
#' * Reconstructing shapes from PCA or other statistical analyses
#' * Filtering out high-frequency noise by using fewer harmonics
#'
#' If `nb_h` is less than the total number of harmonics in the coefficients,
#' only the first `nb_h` harmonics are used (low-pass filtering).
#'
#' @examples
#' # Single outline
#' coo <- matrix(runif(100), ncol = 2)
#' coefs <- efourier(coo, nb_h = 6)
#'
#' # Reconstruct with all harmonics
#' coo_reconstructed <- efourier_i(coefs)
#'
#' # Reconstruct with fewer harmonics (smoothing)
#' coo_smooth <- efourier_i(coefs, nb_h = 3)
#'
#' # From raw output
#' coefs_raw <- efourier(coo, raw = TRUE)
#' efourier_i(coefs_raw)
#'
#' \dontrun{
#' # Tibble workflow
#' library(dplyr)
#' bot %>%
#'   efourier() %>%
#'   efourier_i()
#'
#' # Custom column names
#' bot %>%
#'   efourier(.name = "eft") %>%
#'   efourier_i(.cols = eft, .name = "coo_reconstructed")
#' }
#'
#' @seealso [efourier()] for forward transform
#'
#' @export
efourier_i <- function(x, nb_h = NULL, nb_pts = 120, ..., .cols = NULL, .name = NULL) {

  # Single vector or raw list case
  if (is.numeric(x) || (is.list(x) && !is.data.frame(x) &&
                        all(c("an", "bn", "cn", "dn") %in% names(x)))) {
    return(.efourier_i(x, nb_h = nb_h, nb_pts = nb_pts))
  }

  # List of vectors case
  if (is.list(x) && !is.data.frame(x)) {
    result <- lapply(x, .efourier_i, nb_h = nb_h, nb_pts = nb_pts)
    class(result) <- c("coo", "list")
    return(result)
  }

  # Tibble case
  if (is.data.frame(x)) {
    # Capture .cols with tidyeval
    .cols_quo <- rlang::enquo(.cols)
    if (rlang::quo_is_null(.cols_quo)) {
      # Try to auto-detect coe column
      tryCatch({
        cols_to_process <- get_coe_cols(x, NULL)
        message(sprintf("Using coe column: '%s'", cols_to_process))
      }, error = function(e) {
        stop("Could not auto-detect coe column. Please specify with .cols argument.\n",
             "Original error: ", e$message)
      })
    } else {
      cols_idx <- tidyselect::eval_select(.cols_quo, x)
      cols_to_process <- names(cols_idx)
    }

    # Determine output column name
    col <- cols_to_process[1]
    if (is.null(.name)) {
      .name <- paste0(col, "_i")
    }

    # Check if output column already exists
    if (.name %in% names(x)) {
      stop(sprintf("Column '%s' already exists. Choose a different name with .name argument.", .name))
    }

    # Reconstruct for each row
    result_list <- lapply(x[[col]], .efourier_i, nb_h = nb_h, nb_pts = nb_pts)

    # Add appropriate classes
    class(result_list) <- c("coo", "list")

    # Add to tibble
    x[[.name]] <- result_list

    return(x)
  }

  stop("x must be a vector, list, or tibble")
}


# .efourier_i (internal worker) ----

#' @keywords internal
.efourier_i <- function(x, nb_h = NULL, nb_pts = 120) {
  # If vector, turn it into a list
  if (!is.list(x)) {
    x <- efourier_split(x)
    x$a0 <- 0
    x$c0 <- 0
  }

  # Extract components
  an <- x$an
  bn <- x$bn
  cn <- x$cn
  dn <- x$dn
  a0 <- x$a0
  c0 <- x$c0

  # If missing nb_h, use them all
  if (is.null(nb_h)) {
    nb_h <- length(an)
  } else {
    # Truncate to requested number of harmonics
    nb_h <- min(nb_h, length(an))
    an <- an[1:nb_h]
    bn <- bn[1:nb_h]
    cn <- cn[1:nb_h]
    dn <- dn[1:nb_h]
  }

  # Generate points along the trigonometric circle
  theta <- seq(0, 2 * pi, length = nb_pts + 1)[-(nb_pts + 1)]

  # Prepare matrices to host results
  hx <- matrix(NA, nb_h, nb_pts)
  hy <- matrix(NA, nb_h, nb_pts)

  # Compute each harmonic contribution
  for (i in 1:nb_h) {
    hx[i, ] <- an[i] * cos(i * theta) + bn[i] * sin(i * theta)
    hy[i, ] <- cn[i] * cos(i * theta) + dn[i] * sin(i * theta)
  }

  # Sum harmonics to get x and y coordinates
  x_coords <- (a0 / 2) + apply(hx, 2, sum)
  y_coords <- (c0 / 2) + apply(hy, 2, sum)

  # Make matrix and return with xy class
  result <- cbind(x_coords, y_coords)
  colnames(result) <- c("x", "y")
  class(result) <- c("xy", "matrix")
  result
}


# efourier_norm ----

#' Normalize elliptic Fourier coefficients
#'
#' Normalize EFT coefficients to remove the effects of size, rotation, and
#' starting position, making shapes directly comparable.
#'
#' @param x A numeric vector (EFT coefficients), list of vectors, or tibble
#'   with coe columns.
#' @param start Logical. If `TRUE`, preserves the starting point position.
#'   Default is `FALSE` (removes starting point effect).
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coe objects.
#' @param .name Character. Name for the output coefficient column when `x` is
#'   a tibble. If `NULL`, modifies the column in place. If provided, creates
#'   a new column with this name.
#'
#' @return
#' * If `x` is a vector: returns a normalized coefficient vector with class `c("eft", "numeric")`
#' * If `x` is a list: returns a list of normalized coefficient vectors
#' * If `x` is a tibble: returns the tibble with normalized coefficients
#'   (either modifying in place or creating a new column)
#'
#' @details
#' Normalization removes three sources of variation that are typically not of
#' interest in shape analysis:
#'
#' 1. **Size**: Coefficients are scaled so the first ellipse has unit semi-axis length
#' 2. **Rotation**: Coefficients are rotated so the first ellipse aligns with the x-axis
#' 3. **Starting point** (if `start = FALSE`): The phase of all harmonics is
#'    adjusted to remove the effect of where digitization began
#'
#' After normalization, shapes can be directly compared using multivariate statistics,
#' as remaining variation represents only shape differences.
#'
#' The first harmonic (A1, B1, C1, D1) is used to compute the normalization
#' parameters. For `start = FALSE`, these parameters are:
#' * `theta`: Phase shift to remove starting point effect
#' * `psi`: Rotation angle to align with x-axis
#' * `size`: Scaling factor (inverse of first ellipse semi-axis length)
#'
#' These normalization parameters are stored as attributes on the result for
#' reference but are typically not needed for downstream analysis.
#'
#' @section When to use normalization:
#'
#' * **Always normalize** before multivariate analysis (PCA, LDA, etc.) if you want
#'   to compare shapes independent of size, rotation, and starting point
#' * **Skip normalization** if size or orientation are biologically meaningful
#' * Use `start = TRUE` if the starting point has biological meaning (e.g.,
#'   a specific landmark)
#'
#' @examples
#' # Single outline
#' coo <- matrix(runif(100), ncol = 2)
#' coefs <- efourier(coo, nb_h = 6)
#'
#' # Normalize
#' coefs_norm <- efourier_norm(coefs)
#'
#' # Preserve starting point
#' coefs_norm_start <- efourier_norm(coefs, start = TRUE)
#'
#' # Compare first harmonic before/after
#' coefs[1:4]
#' coefs_norm[1:4]
#'
#' \dontrun{
#' # Typical workflow with tibble
#' library(dplyr)
#' bot %>%
#'   efourier(nb_h = 8) %>%
#'   efourier_norm()  # Normalizes in place
#'
#' # Create new column instead
#' bot %>%
#'   efourier(nb_h = 8) %>%
#'   efourier_norm(.name = "coe_norm")
#' }
#'
#' @references
#' Kuhl, F. P., & Giardina, C. R. (1982). Elliptic Fourier features of a closed
#' contour. Computer graphics and image processing, 18(3), 236-258.
#'
#' @seealso [efourier()] for coefficient computation, [efourier_i()] for
#'   reconstruction.
#'
#' @export
efourier_norm <- function(x, start = FALSE, ..., .cols = NULL, .name = NULL) {

  # Single vector case
  if (is.numeric(x) && !is.matrix(x)) {
    return(.efourier_norm(x, start = start))
  }

  # List case
  if (is.list(x) && !is.data.frame(x)) {
    result <- lapply(x, .efourier_norm, start = start)
    class(result) <- c("eft", "coe", "list")
    return(result)
  }

  # Tibble case
  if (is.data.frame(x)) {
    # Capture .cols with tidyeval
    .cols_quo <- rlang::enquo(.cols)
    if (rlang::quo_is_null(.cols_quo)) {
      # Try to auto-detect coe column
      tryCatch({
        cols_to_process <- get_coe_cols(x, NULL)
      }, error = function(e) {
        stop("Could not auto-detect coe column. Please specify with .cols argument.\n",
             "Original error: ", e$message)
      })
    } else {
      cols_idx <- tidyselect::eval_select(.cols_quo, x)
      cols_to_process <- names(cols_idx)
    }

    col <- cols_to_process[1]

    # Determine if modifying in place or creating new column
    if (is.null(.name)) {
      # Modify in place
      output_col <- col
      message(sprintf("Normalizing '%s' in place", col))
    } else {
      # Create new column
      output_col <- .name
      if (output_col %in% names(x)) {
        stop(sprintf("Column '%s' already exists. Choose a different name.", output_col))
      }
    }

    # Normalize each row
    result_list <- lapply(x[[col]], .efourier_norm, start = start)

    # Add appropriate classes
    class(result_list) <- c("eft", "coe", "list")

    # Add/replace column
    x[[output_col]] <- result_list

    return(x)
  }

  stop("x must be a numeric vector, list, or tibble")
}


# .efourier_norm (internal worker) ----

#' @keywords internal
.efourier_norm <- function(x, start = FALSE) {
  # Split vector into components
  ef <- efourier_split(x)

  # Extract first harmonic
  A1 <- ef$an[1]
  B1 <- ef$bn[1]
  C1 <- ef$cn[1]
  D1 <- ef$dn[1]

  nb_h <- length(ef$an)

  # Calculate phase shift (theta) to remove starting point effect
  theta <- 0.5 * atan(2 * (A1 * B1 + C1 * D1) /
                        (A1^2 + C1^2 - B1^2 - D1^2)) %% pi

  # Apply phase shift
  phaseshift <- matrix(c(cos(theta), sin(theta),
                         -sin(theta), cos(theta)), 2, 2)

  M2 <- matrix(c(A1, C1, B1, D1), 2, 2) %*% phaseshift

  # Determine correct orientation
  v <- apply(M2^2, 2, sum)
  if (v[1] < v[2]) {
    theta <- theta + pi/2
  }
  theta <- (theta + pi/2) %% pi - pi/2

  # Calculate normalized first harmonic
  Aa <- A1 * cos(theta) + B1 * sin(theta)
  Cc <- C1 * cos(theta) + D1 * sin(theta)

  # Calculate size (scaling factor)
  scale <- sqrt(Aa^2 + Cc^2)

  # Calculate rotation angle (psi)
  psi <- atan(Cc / Aa) %% pi
  if (Aa < 0) {
    psi <- psi + pi
  }

  # Normalization parameters
  size <- 1 / scale
  rotation <- matrix(c(cos(psi), -sin(psi),
                       sin(psi), cos(psi)), 2, 2)

  # Prepare output vectors
  A <- B <- C <- D <- numeric(nb_h)

  # If start = TRUE, don't remove starting point effect
  if (start) {
    theta <- 0
  }

  # Apply normalization to all harmonics
  for (i in 1:nb_h) {
    mat <- size * rotation %*%
      matrix(c(ef$an[i], ef$cn[i],
               ef$bn[i], ef$dn[i]), 2, 2) %*%
      matrix(c(cos(i * theta), sin(i * theta),
               -sin(i * theta), cos(i * theta)), 2, 2)

    A[i] <- mat[1, 1]
    B[i] <- mat[1, 2]
    C[i] <- mat[2, 1]
    D[i] <- mat[2, 2]
  }

  # Create output vector in standard format
  result <- c(A, B, C, D)
  result <- efourier_name(result)
  class(result) <- c("eft", "numeric")

  # Store normalization parameters as attributes
  attr(result, "size") <- scale
  attr(result, "theta") <- theta
  attr(result, "psi") <- psi

  result
}

# efourier_name ----

#' Name elliptic Fourier coefficients
#'
#' Add standard names to an EFT coefficient vector.
#'
#' @param x Numeric vector of EFT coefficients (length must be divisible by 4).
#'
#' @return Named numeric vector with names in the format A1, A2, ..., An,
#'   B1, B2, ..., Bn, C1, ..., D1, ...
#'
#' @details
#' Names are assigned in the order: An (x cosine), Bn (x sine), Cn (y cosine),
#' Dn (y sine), where n is the harmonic number.
#'
#' @examples
#' # Create unnamed coefficient vector
#' coefs <- runif(24)  # 6 harmonics
#' efourier_name(coefs)
#'
#' @export
efourier_name <- function(x) {
  n <- floor(length(x) / 4)
  names(x) <- paste0(
    rep(LETTERS[1:4], each = n),  # coefficient name
    rep(1:n, times = 4)            # harmonic position
  )
  x
}


# efourier_split ----

#' Split EFT coefficient vector into components
#'
#' Separate a flattened EFT coefficient vector into its An, Bn, Cn, Dn components.
#'
#' @param x Numeric vector of EFT coefficients (length must be divisible by 4).
#'
#' @return List with four elements:
#' * `an`: An coefficients (x-axis cosine terms)
#' * `bn`: Bn coefficients (x-axis sine terms)
#' * `cn`: Cn coefficients (y-axis cosine terms)
#' * `dn`: Dn coefficients (y-axis sine terms)
#'
#' @examples
#' # Create coefficient vector
#' coefs <- efourier(matrix(runif(100), ncol = 2), nb_h = 6)
#' efourier_split(coefs)
#'
#' @seealso [efourier()] to generate coefficients
#'
#' @export
efourier_split <- function(x) {
  # Deduce harmonic number
  nb_h <- length(x) / 4

  # Start and ending position within the vector
  beg <- nb_h * (0:3) + 1
  end <- nb_h * (1:4)

  # Split into list and return
  list(
    an = x[beg[1]:end[1]],
    bn = x[beg[2]:end[2]],
    cn = x[beg[3]:end[3]],
    dn = x[beg[4]:end[4]]
  )
}

