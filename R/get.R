# make_get_function ----

#' Create a get function with automatic dispatch
#'
#' Higher-order function factory that creates get_ functions with automatic
#' dispatch to handle single matrices, lists of matrices, and tibbles.
#'
#' @param impl_fn Function. The implementation function that operates on a single
#'   matrix (nx2). Should accept `x` as first argument and `...` for additional args.
#' @param fn_name Character. Optional name for debugging (not used functionally).
#'
#' @return A function that dispatches based on input type.
#'
#' @details
#' The returned function automatically:
#' - Applies impl_fn to single matrices and returns the result
#' - Applies impl_fn to each element of a list and returns a list
#' - Applies impl_fn to tibble coo columns and EXTRACTS results (does not modify tibble)
#'
#' For tibbles, get_* functions extract values for further processing by the user.
#' They do NOT create new columns or modify the tibble.
#'
#' Additional arguments are passed through via `...`
#'
#' @keywords internal
#' @noRd
make_get_function <- function(impl_fn, fn_name = NULL) {
  # Infer function name if not provided
  if (is.null(fn_name)) {
    fn_name <- deparse(substitute(impl_fn))
  }

  f <- function(x, ..., .cols = NULL) {
    # Single matrix case - return computed value
    if (is.matrix(x)) {
      return(impl_fn(x, ...))
    }

    # List case - return list of computed values
    if (is.list(x) && !is.data.frame(x)) {
      return(lapply(x, impl_fn, ...))
    }

    # Tibble case - EXTRACT values, don't modify tibble
    if (is.data.frame(x)) {
      # Capture .cols with tidyeval
      .cols_quo <- rlang::enquo(.cols)
      if (rlang::quo_is_null(.cols_quo)) {
        cols_to_process <- get_coo_cols(x, NULL)
      } else {
        cols_idx <- tidyselect::eval_select(.cols_quo, x)
        cols_to_process <- names(cols_idx)
      }

      # Extract from first (or only) coo column
      col <- cols_to_process[1]
      result <- lapply(x[[col]], impl_fn, ...)

      # Simplify if all results are scalars
      if (all(sapply(result, length) == 1)) {
        result <- unlist(result)
      }

      return(result)
    }
  }

  # Store metadata for display
  attr(f, "impl_fn") <- fn_name

  # Custom class for display
  class(f) <- c("momocs2_get_function", "function")

  return(f)
}


# print.momocs2_get_function ----

#' @export
print.momocs2_get_function <- function(x, ...) {
  impl_fn <- attr(x, "impl_fn")
  cat(sprintf("# Momocs2 get function wrapping: %s\n", impl_fn))
  cat(sprintf("# View implementation: Momocs2:::%s\n\n", impl_fn))
  NextMethod()
}


# get_centroid ----

#' Get centroid of a shape
#'
#' Calculate the centroid (center of mass) of a shape.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns a numeric vector of length 2 (x, y coordinates)
#' * If `x` is a list: returns a list of numeric vectors
#' * If `x` is a tibble: returns a list of numeric vectors extracted from coo column
#'
#' @details
#' For tibbles, this function extracts centroid values without modifying the tibble.
#' Use within `mutate()` to add as a column: `mutate(df, cent = get_centroid(coo))`
#'
#' @examples
#' get_centroid(shapes$cat)
#' get_centroid(shapes)
#'
#' # Extract from tibble
#' centroids <- get_centroid(bot)
#'
#' # Add to tibble
#' bot$centroid <- get_centroid(bot)
#'
#' @seealso [coo_center()] for centering shapes
#'
#' @keywords internal
#' @export
get_centroid <- make_get_function(.get_centroid)

#' @rdname get_centroid
#' @export
centroid <- get_centroid

.get_centroid <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  colMeans(x)
}


# get_area ----

#' Get area of a closed outline
#'
#' Calculate the area enclosed by a shape using the shoelace formula.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns a numeric scalar
#' * If `x` is a list: returns a numeric vector
#' * If `x` is a tibble: returns a numeric vector extracted from coo column
#'
#' @details
#' Uses the shoelace formula (also called surveyor's formula) to compute area.
#' The outline is automatically treated as closed (first point connects to last).
#'
#' For tibbles, this function extracts values without modifying the tibble.
#' Use within `mutate()` to add as a column, or use `measure("area")` for convenience.
#'
#' @examples
#' get_area(shapes$cat)
#' get_area(shapes)
#'
#' # Extract from tibble
#' areas <- get_area(bot)
#'
#' # Add to tibble
#' bot$area <- get_area(bot)
#'
#' # Or use measure() for convenience
#' bot %>% measure("area")
#'
#' @seealso [measure()] for adding measurement columns; [get_perim()] for perimeter
#'
#' @keywords internal
#' @export
get_area <- make_get_function(.get_area)

.get_area <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  n <- nrow(x)
  # Shoelace formula - vectorized
  x1 <- x[, 1]
  y1 <- x[, 2]
  x2 <- c(x1[-1], x1[1])  # shift x coordinates (wrap around)
  y2 <- c(y1[-1], y1[1])  # shift y coordinates (wrap around)
  abs(0.5 * sum(x1 * y2 - x2 * y1))
}


# get_circularity ----

#' Get circularity measures
#'
#' Calculate circularity (isoperimetric quotient) and related shape descriptors.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns a numeric scalar
#' * If `x` is a list: returns a numeric vector
#' * If `x` is a tibble: returns a numeric vector extracted from coo column
#'
#' @details
#' **Circularity measures:**
#' - `get_circularity()`: Isoperimetric quotient = 4π × Area / Perimeter².
#'   Value of 1 for a perfect circle, < 1 for other shapes.
#' - `get_circularity_norm()`: Normalized version = Area / (Perimeter²/(4π)).
#'   Alternative formulation, same interpretation.
#' - `get_circularity_haralick()`: Mean(radii) / SD(radii) from centroid.
#'   Higher values = more circular. Sensitive to irregularities.
#'
#' All measures equal 1 for perfect circles and decrease for other shapes.
#'
#' @examples
#' get_circularity(shapes$cat)
#' get_circularity_norm(shapes$cat)
#' get_circularity_haralick(shapes$cat)
#'
#' # Use in measure()
#' bot %>% measure("circularity")
#' bot %>% measure(c("circularity", "circularity_haralick"))
#'
#' @name get_circularity
#' @seealso [measure()] for adding measurement columns
#' @keywords internal
NULL

#' @rdname get_circularity
#' @export
get_circularity <- make_get_function(.get_circularity)

.get_circularity <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  area <- .get_area(x)
  perim <- .get_perim(x)
  (4 * pi * area) / (perim^2)
}

#' @rdname get_circularity
#' @export
get_circularity_norm <- make_get_function(.get_circularity_norm)

.get_circularity_norm <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  area <- .get_area(x)
  perim <- .get_perim(x)
  area / (perim^2 / (4 * pi))
}

#' @importFrom stats sd
#' @rdname get_circularity
#' @export
get_circularity_haralick <- make_get_function(.get_circularity_haralick)

.get_circularity_haralick <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  # Center shape
  coo_centered <- .coo_center(x)
  # Calculate radii (distances from centroid to each point)
  radii <- sqrt(rowSums(coo_centered^2))
  # Mean / SD
  mean(radii) / sd(radii)
}


# get_lw ----

#' Get length and width
#'
#' Calculate length and width based on inertia axes (PCA alignment).
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns a numeric vector c(length, width)
#' * If `x` is a list: returns a list of numeric vectors
#' * If `x` is a tibble: returns a list of numeric vectors extracted from coo column
#'
#' @details
#' Length is defined as the range along the major inertia axis (largest variance).
#' Width is defined as the range along the minor inertia axis (smallest variance).
#' Shape is first centered and aligned using SVD before computing ranges.
#'
#' Note: Returns a vector, not a scalar, so cannot be used with `measure()`.
#' Use `get_length()`, `get_width()`, or `get_elongation()` for scalar measurements.
#'
#' @examples
#' get_lw(shapes$cat)
#' get_lw(shapes)
#'
#' @seealso [get_length()], [get_width()], [get_elongation()]
#'
#' @keywords internal
#' @export
get_lw <- make_get_function(.get_lw)

.get_lw <- function(x, ...) {
  if (!is.matrix(x)) return(c(NA_real_, NA_real_))

  # Center shape
  coo_centered <- .coo_center(x)

  # SVD for principal axes
  svd_res <- svd(coo_centered)

  # Project onto principal axes
  coo_aligned <- coo_centered %*% svd_res$v

  # Ranges along each axis
  length <- diff(range(coo_aligned[, 1]))
  width <- diff(range(coo_aligned[, 2]))

  c(length = length, width = width)
}


# get_length ----

#' Get length
#'
#' Calculate length along the major inertia axis.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns a numeric scalar
#' * If `x` is a list: returns a numeric vector
#' * If `x` is a tibble: returns a numeric vector extracted from coo column
#'
#' @details
#' Length is the range along the major inertia axis (largest variance direction).
#'
#' @examples
#' get_length(shapes$cat)
#'
#' # Use in measure()
#' bot %>% measure("length")
#'
#' @seealso [get_width()], [get_lw()], [get_elongation()]
#'
#' @keywords internal
#' @export
get_length <- make_get_function(.get_length)

.get_length <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  .get_lw(x)[1]
}


# get_width ----

#' Get width
#'
#' Calculate width along the minor inertia axis.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns a numeric scalar
#' * If `x` is a list: returns a numeric vector
#' * If `x` is a tibble: returns a numeric vector extracted from coo column
#'
#' @details
#' Width is the range along the minor inertia axis (smallest variance direction).
#'
#' @examples
#' get_width(shapes$cat)
#'
#' # Use in measure()
#' bot %>% measure("width")
#'
#' @seealso [get_length()], [get_lw()], [get_elongation()]
#'
#' @keywords internal
#' @export
get_width <- make_get_function(.get_width)

.get_width <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  .get_lw(x)[2]
}


# get_elongation ----

#' Get elongation
#'
#' Calculate elongation (aspect ratio) as length / width.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns a numeric scalar
#' * If `x` is a list: returns a numeric vector
#' * If `x` is a tibble: returns a numeric vector extracted from coo column
#'
#' @details
#' Elongation is the aspect ratio based on inertia axes: length / width.
#' Value of 1 indicates equal length and width. Higher values indicate more elongated shapes.
#'
#' This is equivalent to eccentricity based on bounding box dimensions.
#'
#' @examples
#' get_elongation(shapes$cat)
#'
#' # Use in measure()
#' bot %>% measure("elongation")
#'
#' @seealso [get_length()], [get_width()], [get_rectangularity()]
#'
#' @keywords internal
#' @export
get_elongation <- make_get_function(.get_elongation)

.get_elongation <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  lw <- .get_lw(x)
  lw[1] / lw[2]
}


# get_rectangularity ----

#' Get rectangularity
#'
#' Calculate rectangularity as the ratio of area to bounding box area.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns a numeric scalar
#' * If `x` is a list: returns a numeric vector
#' * If `x` is a tibble: returns a numeric vector extracted from coo column
#'
#' @details
#' Rectangularity = Area / (Length × Width).
#' Value of 1 for a perfect rectangle. Lower values indicate less rectangular shapes.
#'
#' @examples
#' get_rectangularity(shapes$cat)
#'
#' # Use in measure()
#' bot %>% measure("rectangularity")
#'
#' @seealso [get_length()], [get_width()], [get_elongation()]
#'
#' @keywords internal
#' @export
get_rectangularity <- make_get_function(.get_rectangularity)

.get_rectangularity <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  area <- .get_area(x)
  lw <- .get_lw(x)
  area / (lw[1] * lw[2])
}


# get_chull ----

#' Get convex hull
#'
#' Calculate the convex hull coordinates or indices.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * `get_chull()`: convex hull coordinates as a matrix
#' * `get_chull_id()`: indices of convex hull points as an integer vector
#'
#' @details
#' Uses `grDevices::chull()` to compute the convex hull.
#' The convex hull is the smallest convex polygon containing all points.
#'
#' Note: Returns non-scalar values, so cannot be used with `measure()`.
#'
#' @examples
#' get_chull(shapes$cat)
#' get_chull_id(shapes$cat)
#'
#' @name get_chull
#' @seealso [get_convexity()], [get_solidity()]
#' @keywords internal
NULL

#' @rdname get_chull
#' @export
get_chull <- make_get_function(.get_chull)

.get_chull <- function(x, ...) {
  if (!is.matrix(x)) return(matrix(NA_real_, 0, 2))
  ids <- grDevices::chull(x)
  x[ids, ]
}

#' @rdname get_chull
#' @export
get_chull_id <- make_get_function(.get_chull_id)

.get_chull_id <- function(x, ...) {
  if (!is.matrix(x)) return(integer(0))
  grDevices::chull(x)
}


# get_convexity ----

#' Get convexity
#'
#' Calculate convexity as the ratio of convex hull perimeter to shape perimeter.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns a numeric scalar
#' * If `x` is a list: returns a numeric vector
#' * If `x` is a tibble: returns a numeric vector extracted from coo column
#'
#' @details
#' Convexity = Perimeter(convex_hull) / Perimeter(shape).
#' Value of 1 for convex shapes. Lower values indicate concavities.
#'
#' @examples
#' get_convexity(shapes$cat)
#'
#' # Use in measure()
#' bot %>% measure("convexity")
#'
#' @seealso [get_solidity()], [get_chull()]
#'
#' @keywords internal
#' @export
get_convexity <- make_get_function(.get_convexity)

.get_convexity <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  chull_coo <- .get_chull(x)
  perim_chull <- .get_perim(chull_coo)
  perim <- .get_perim(x)
  perim_chull / perim
}


# get_solidity ----

#' Get solidity
#'
#' Calculate solidity as the ratio of shape area to convex hull area.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns a numeric scalar
#' * If `x` is a list: returns a numeric vector
#' * If `x` is a tibble: returns a numeric vector extracted from coo column
#'
#' @details
#' Solidity = Area(shape) / Area(convex_hull).
#' Value of 1 for convex shapes. Lower values indicate concavities or holes.
#'
#' @examples
#' get_solidity(shapes$cat)
#'
#' # Use in measure()
#' bot %>% measure("solidity")
#'
#' @seealso [get_convexity()], [get_chull()]
#'
#' @keywords internal
#' @export
get_solidity <- make_get_function(.get_solidity)

.get_solidity <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  area <- .get_area(x)
  chull_coo <- .get_chull(x)
  area_chull <- .get_area(chull_coo)
  area / area_chull
}


# get_calliper ----

#' Get calliper
#'
#' Calculate the maximum distance between any two points (calliper/Feret diameter).
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * `get_calliper()`: maximum distance as a numeric scalar
#' * `get_calliper_ids()`: indices of the two furthest points as a vector c(i, j)
#'
#' @details
#' Calculates the maximum distance between any two points on the outline.
#' Also known as Feret diameter or maximum calliper distance.
#'
#' Uses brute force O(n²) algorithm. May be slow for outlines with many points.
#'
#' Note: `get_calliper_ids()` returns a vector, not a scalar, so cannot be used with `measure()`.
#'
#' @examples
#' get_calliper(shapes$cat)
#' get_calliper_ids(shapes$cat)
#'
#' # Use in measure()
#' bot %>% measure("calliper")
#'
#' @name get_calliper
#' @keywords internal
NULL

#' @rdname get_calliper
#' @export
get_calliper <- make_get_function(.get_calliper)

.get_calliper <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  n <- nrow(x)
  if (n < 2) return(0)

  max_dist <- 0
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      dist <- sqrt((x[i, 1] - x[j, 1])^2 + (x[i, 2] - x[j, 2])^2)
      if (dist > max_dist) {
        max_dist <- dist
      }
    }
  }
  max_dist
}

#' @rdname get_calliper
#' @export
get_calliper_ids <- make_get_function(.get_calliper_ids)

.get_calliper_ids <- function(x, ...) {
  if (!is.matrix(x)) return(c(NA_integer_, NA_integer_))
  n <- nrow(x)
  if (n < 2) return(c(1L, 1L))

  max_dist <- 0
  ids <- c(1L, 1L)

  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      dist <- sqrt((x[i, 1] - x[j, 1])^2 + (x[i, 2] - x[j, 2])^2)
      if (dist > max_dist) {
        max_dist <- dist
        ids <- c(i, j)
      }
    }
  }
  ids
}


# get_rectilinearity ----

#' Get rectilinearity
#'
#' Calculate rectilinearity - a measure of how well a shape fits a rectangle
#' after optimal rotation.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns a numeric scalar
#' * If `x` is a list: returns a numeric vector
#' * If `x` is a tibble: returns a numeric vector extracted from coo column
#'
#' @details
#' Rectilinearity measures how rectangular a shape is by testing multiple
#' rotations and finding the orientation that best fits a rectangle.
#'
#' Algorithm from Zunic & Rosin (2003). Higher values indicate more rectangular shapes.
#' Computationally intensive - tests 4n rotations where n is the number of points.
#'
#' @examples
#' get_rectilinearity(shapes$cat)
#'
#' # Use in measure()
#' bot %>% measure("rectilinearity")
#'
#' @references
#' Zunic, J., & Rosin, P. L. (2003). Rectilinearity measurements for polygons.
#' IEEE Transactions on Pattern Analysis and Machine Intelligence.
#'
#' @keywords internal
#' @export
get_rectilinearity <- make_get_function(.get_rectilinearity)

.get_rectilinearity <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)

  # Ensure closed outline
  if (!isTRUE(all.equal(x[1, ], x[nrow(x), ]))) {
    coo_c <- rbind(x, x[1, ])
    coo <- x
  } else {
    coo_c <- x
    coo <- x[-nrow(x), ]
  }

  n <- nrow(coo)
  k <- 4 * n

  # Helper functions for distances
  l1 <- function(x1, y1, x2, y2) {
    abs(x1 - x2) + abs(y1 - y2)
  }

  l2 <- function(x1, y1, x2, y2) {
    sqrt((x1 - x2)^2 + (y1 - y2)^2)
  }

  # Calculate edge distances
  l1.e <- l2.e <- numeric(n)
  for (i in 1:n) {
    x1 <- coo_c[i, 1]
    y1 <- coo_c[i, 2]
    x2 <- coo_c[i + 1, 1]
    y2 <- coo_c[i + 1, 2]
    l1.e[i] <- l1(x1, y1, x2, y2)
    l2.e[i] <- l2(x1, y1, x2, y2)
  }

  # Calculate edge angles
  theta <- numeric(n)
  for (i in 1:n) {
    dx <- coo_c[i + 1, 1] - coo_c[i, 1]
    dy <- coo_c[i + 1, 2] - coo_c[i, 2]
    theta[i] <- atan2(dy, dx)
  }

  # Generate test angles
  theta.k <- abs(c(
    theta - pi/2,
    theta - pi,
    theta - 3*pi/2,
    theta - 2*pi
  ))
  alpha.k <- sort(theta.k)

  # Calculate projections for each rotation
  P1.Pa <- numeric(k)
  for (j in 1:k) {
    P1.Pa_n <- numeric(n)
    for (i in 1:n) {
      cos.ij <- cos(theta[i] + alpha.k[j])
      sin.ij <- sin(theta[i] + alpha.k[j])
      a.ij <- ifelse(cos.ij > 0, l2.e[i], -l2.e[i])
      b.ij <- ifelse(sin.ij > 0, l2.e[i], -l2.e[i])
      P1.Pa_n[i] <- a.ij * cos.ij + b.ij * sin.ij
    }
    P1.Pa[j] <- sum(P1.Pa_n)
  }

  # Return normalized rectilinearity
  (4 / (4 - pi)) * ((sum(l2.e) / min(P1.Pa)) - (pi / 4))
}



# get_centroid_size ----

#' Get centroid size
#'
#' Calculate the centroid size (CS): the square root of the sum of squared
#' distances from each point to the centroid.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns a numeric scalar
#' * If `x` is a list: returns a numeric vector
#' * If `x` is a tibble: returns a numeric vector extracted from coo column
#'
#' @details
#' Centroid size is a common size measure in geometric morphometrics.
#' It is scale-independent and used for allometric correction.
#'
#' For tibbles, this function extracts values without modifying the tibble.
#' Use within `mutate()` to add as a column: `mutate(df, cs = get_centroid_size(coo))`
#'
#' @examples
#' get_centroid_size(shapes$cat)
#' get_centroid_size(shapes)
#'
#' # Extract from tibble
#' sizes <- get_centroid_size(bot)
#'
#' # Add to tibble
#' bot$centsize <- get_centroid_size(bot)
#'
#' @keywords internal
#' @export
get_centroid_size <- make_get_function(.get_centroid_size)

#' @rdname get_centroid_size
#' @export
centsize <- get_centroid_size

.get_centroid_size <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  cent <- .get_centroid(x)
  sqrt(sum(sweep(x, 2, cent, "-")^2))
}


# get_centroid_size_norm ----

#' Get normalized centroid size
#'
#' Calculate the normalized centroid size (centroid size divided by perimeter).
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns a numeric scalar
#' * If `x` is a list: returns a numeric vector
#' * If `x` is a tibble: returns a numeric vector extracted from coo column
#'
#' @details
#' For tibbles, this function extracts values without modifying the tibble.
#' Use within `mutate()` to add as a column: `mutate(df, cs_norm = get_centroid_size_norm(coo))`
#'
#' @examples
#' get_centroid_size_norm(shapes$cat)
#' get_centroid_size_norm(shapes)
#'
#' # Extract from tibble
#' sizes_norm <- get_centroid_size_norm(bot)
#'
#' # Add to tibble
#' bot$cs_norm <- get_centroid_size_norm(bot)
#'
#' @keywords internal
#' @export
get_centroid_size_norm <- make_get_function(.get_centroid_size_norm)

#' @rdname get_centroid_size_norm
#' @export
centsize_norm <- get_centroid_size_norm

.get_centroid_size_norm <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  cent <- .get_centroid(x)
  sqrt(mean(rowSums(sweep(x, 2, cent, "-")^2)))
}


# get_perim ----

#' Get perimeter measurements
#'
#' Calculate perimeter-related measurements of a shape.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @details
#' - `get_perim()`: total perimeter
#' - `get_perim_along()`: length of each segment
#' - `get_perim_cum()`: cumulative perimeter from start
#'
#' For tibbles, these functions extract values without modifying the tibble.
#'
#' @return
#' * `get_perim()`: numeric scalar (matrix) or vector (list/tibble)
#' * `get_perim_along()`: numeric vector (matrix) or list of vectors (list/tibble)
#' * `get_perim_cum()`: numeric vector (matrix) or list of vectors (list/tibble)
#'
#' @examples
#' get_perim(shapes$cat)
#' get_perim_along(shapes$cat)
#' get_perim_cum(shapes$cat)
#'
#' # Extract from tibble
#' perims <- get_perim(bot)
#'
#' # Add to tibble
#' bot$perim <- get_perim(bot)
#'
#' @name get_perim
#' @keywords internal
NULL

#' @rdname get_perim
#' @export
get_perim <- make_get_function(.get_perim)

#' @rdname get_perim
#' @export
get_perim_along <- make_get_function(.get_perim_along)

#' @rdname get_perim
#' @export
get_perim_cum <- make_get_function(.get_perim_cum)

.get_perim <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  .get_perim_along(x) %>% sum()
}

.get_perim_along <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  sqrt(rowSums(diff(rbind(x, x[1,]))^2))
}

.get_perim_cum <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  .get_perim_along(x) %>% cumsum()
}


# get_angles ----

#' Get angles from centroid
#'
#' Calculate the angle from the centroid to each point on the shape.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns a numeric vector of angles in radians
#' * If `x` is a list: returns a list of numeric vectors
#' * If `x` is a tibble: returns a list of numeric vectors extracted from coo column
#'
#' @details
#' Angles are computed using `atan2()` on centered coordinates.
#' Useful for angular resampling or finding points in specific directions.
#'
#' For tibbles, this function extracts values without modifying the tibble.
#'
#' @examples
#' get_angles(shapes$cat)
#'
#' # Extract from tibble
#' angles <- get_angles(bot)
#'
#' # Add to tibble
#' bot$angles <- get_angles(bot)
#'
#' @keywords internal
#' @export
get_angles <- make_get_function(.get_angles)

.get_angles <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  # center
  coo <- .coo_center(x)
  # angles to each point
  atan2(coo[, 2], coo[, 1])
}


# get_ed ----

#' Euclidean distance measurements
#'
#' Calculate Euclidean distances between points or shapes.
#'
#' @param x A matrix (nx2) or numeric vector of length 2.
#' @param y A matrix (nx2) or numeric vector of length 2.
#' @param r Numeric. Interpolation parameter between 0 and 1.
#'
#' @details
#' - `get_ed()`: distance between two points or total distance between shapes
#' - `get_ed_along()`: point-wise distances between two conformable matrices
#' - `get_ed_intermediate()`: interpolated point(s) along line between x and y
#'
#' These functions do not use the `make_get_function` dispatcher as they require
#' two arguments (x and y).
#'
#' @return
#' * `get_ed()`: numeric scalar
#' * `get_ed_along()`: numeric vector
#' * `get_ed_intermediate()`: matrix (same dimensions as inputs)
#'
#' @examples
#' p1 <- c(0, 0)
#' p2 <- c(3, 4)
#' get_ed(p1, p2)
#'
#' get_ed_along(shapes$cat, shapes$dog)
#' get_ed_intermediate(p1, p2, r = 0.5)
#'
#' @name get_ed
#' @keywords internal
NULL

#' @rdname get_ed
#' @export
get_ed <- function(x, y){
  sqrt(sum((x-y)^2))
}

#' @rdname get_ed
#' @export
get_ed_along <- function(x, y){
  sqrt(rowSums((x-y)^2))
}

#' @rdname get_ed
#' @export
get_ed_intermediate <- function(x, y, r){
  r * (y - x) + x
}


# get_closest ----

#' Find closest point to target
#'
#' Find the point on a shape closest to a target location or angle.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param target A numeric vector of length 2 (x, y) or a list with `$x` and `$y`
#'   (e.g., from `locator()`).
#' @param theta Numeric. Angle in radians for angular search.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns an integer (row index of closest point)
#' * If `x` is a list: returns a numeric vector of integers
#' * If `x` is a tibble: returns a numeric vector of integers extracted from coo column
#'
#' @details
#' - `get_closest()`: finds point closest to spatial target using Euclidean distance.
#'   Compatible with `locator()` for interactive selection.
#' - `get_closest_angle()`: finds point closest to a specified angle direction.
#'
#' For tibbles, these functions extract values without modifying the tibble.
#'
#' @examples
#' get_closest(shapes$cat, c(200, 100))
#' get_closest_angle(shapes$cat, theta = 0)
#'
#' # Extract from tibble
#' closest_ids <- get_closest(bot, c(100, 100))
#'
#' # Add to tibble
#' bot$closest <- get_closest(bot, c(100, 100))
#'
#' @name get_closest
#' @keywords internal
NULL

#' @rdname get_closest
#' @export
get_closest <- make_get_function(.get_closest)

#' @rdname get_closest
#' @export
get_closest_angle <- make_get_function(.get_closest_angle)

.get_closest <- function(x, target, ...) {
  if (!is.matrix(x)) return(NA_integer_)
  # in case it comes from locator
  target <- unlist(target)
  # minimal euclidean distance id
  dx <- (x[, 1] - target[1])^2
  dy <- (x[, 2] - target[2])^2
  which.min(dx+dy)
}

.get_closest_angle <- function(x, theta = 0, ...) {
  if (!is.matrix(x)) return(NA_integer_)
  # angle to each point
  angles <- .get_angles(x)
  # closest angle to target
  angle_diff <- abs(angles - theta)
  angle_diff <- pmin(angle_diff, 2*pi - angle_diff)
  # index of closest point
  which.min(angle_diff)
}


# get_ldk ----

#' Extract landmark coordinates
#'
#' Extract the coordinates at landmark positions from shapes.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ldk Integer vector. Landmark indices (for single matrix or list).
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#' @param .ldk_col Character. Name of landmark column. If `NULL`, uses `colname_ldk`.
#' @param .name Character. Name for the output column when `x` is a tibble. If `NULL`,
#'   uses "colname_ldk_coords" (e.g., "coo_ldk_coords").
#'
#' @return
#' * If `x` is a single matrix: returns a matrix with landmark coordinates
#' * If `x` is a list: returns a list of matrices with landmark coordinates
#' * If `x` is a tibble: returns the tibble with new landmark coordinates column added
#'
#' @details
#' For tibbles, this function automatically finds the landmark column using the
#' naming convention `colname_ldk` (e.g., `coo_ldk` for a `coo` column).
#'
#' Unlike other `get_*` functions, `get_ldk()` DOES add a column to tibbles for
#' convenience, since landmark coordinates are commonly needed alongside the original
#' coordinates.
#'
#' @examples
#' # Single matrix with landmark indices
#' get_ldk(shapes$cat, ldk = c(1, 10, 50, 100))
#'
#' # List with single landmark vector (applied to all)
#' get_ldk(shapes, ldk = c(1, 25, 50))
#'
#' # Tibble - auto-detect landmark column and add coordinates
#' bot %>% get_ldk()
#'
#' # Tibble with custom output name
#' bot %>% get_ldk(.name = "landmarks")
#'
#' @keywords internal
#' @export
get_ldk <- function(x, ldk = NULL, ..., .cols = NULL, .ldk_col = NULL, .name = NULL) {
  # Single matrix case
  if (is.matrix(x)) {
    if (is.null(ldk) || length(ldk) == 0) {
      return(matrix(numeric(0), ncol = 2))
    }
    return(x[ldk, , drop = FALSE])
  }

  # List case
  if (is.list(x) && !is.data.frame(x)) {
    if (is.null(ldk)) {
      stop("For list input, 'ldk' must be provided (either a vector or a list)")
    }

    # If ldk is a single vector, apply to all shapes
    if (!is.list(ldk)) {
      ldk <- rep(list(ldk), length(x))
    }

    # Extract landmarks from each shape
    result <- Map(function(coo, ldk_idx) {
      if (is.null(ldk_idx) || length(ldk_idx) == 0) {
        return(matrix(numeric(0), ncol = 2))
      }
      coo[ldk_idx, , drop = FALSE]
    }, x, ldk)

    return(result)
  }

  # Tibble case - adds column (special case for landmarks)
  if (is.data.frame(x)) {
    # Determine coo column(s) with tidyeval
    .cols_quo <- rlang::enquo(.cols)
    if (rlang::quo_is_null(.cols_quo)) {
      cols_to_process <- get_coo_cols(x, NULL)
    } else {
      cols_idx <- tidyselect::eval_select(.cols_quo, x)
      cols_to_process <- names(cols_idx)
    }

    # Process each coo column
    for (col in cols_to_process) {
      # Determine landmark column name with tidyeval support
      .ldk_col_quo <- rlang::enquo(.ldk_col)
      ldk_col_name <- if (!rlang::quo_is_null(.ldk_col_quo)) {
        rlang::as_name(.ldk_col_quo)
      } else {
        paste0(col, "_ldk")
      }

      # Check if landmark column exists
      if (!ldk_col_name %in% names(x)) {
        warning(sprintf("Landmark column '%s' not found. Skipping.", ldk_col_name))
        next
      }

      # Determine output column name
      if (is.null(.name)) {
        out_col_name <- paste0(col, "_ldk_coords")
      } else {
        out_col_name <- .name
      }

      # Extract landmark coordinates for each shape
      x[[out_col_name]] <- Map(function(coo, ldk_idx) {
        if (is.null(ldk_idx) || length(ldk_idx) == 0) {
          return(matrix(numeric(0), ncol = 2))
        }
        coo[ldk_idx, , drop = FALSE]
      }, x[[col]], x[[ldk_col_name]])
    }

    return(x)
  }

  stop("x must be a matrix, list, or tibble")
}
