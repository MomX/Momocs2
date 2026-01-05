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
#'\dontrun{
#'  # Add to tibble when dplyr is loaded
#' bot %>% mutate(centroid = get_centroid(coo))
#'}
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
#'\dontrun{
#'  # Add to tibble when dplyr is loaded
#' bot %>% mutate(centsize = get_centroid_size(coo))
#'}
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
#'\dontrun{
#'  # Add to tibble when dplyr is loaded
#' bot %>% mutate(cs_norm = get_centroid_size_norm(coo))
#'}
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
#'\dontrun{
#'  # Add to tibble when dplyr is loaded
#' bot %>% mutate(perim = get_perim(coo))
#'}
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
#'\dontrun{
#'  # Add to tibble when dplyr is loaded
#' bot %>% mutate(angles = get_angles(coo))
#'}
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
#'\dontrun{
#'  # Add to tibble when dplyr is loaded
#' bot %>% mutate(closest = get_closest(coo, c(100, 100)))
#'}
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
