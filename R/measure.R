# as_meas ----

#' Convert to measurement class
#'
#' Add "meas" class to a vector of measurements for better printing.
#'
#' @param x A numeric vector of measurements.
#'
#' @return The input with class "meas" added.
#'
#' @keywords internal
#' @export
as_meas <- function(x) {
  class(x) <- c("meas", class(x))
  x
}

#' @export
print.meas <- function(x, ...) {
  cat("# Measurements (n =", length(x), ")\n")
  if (length(x) <= 10) {
    print(unclass(x))
  } else {
    cat("# Showing first 10 of", length(x), "\n")
    print(unclass(x[1:10]))
    cat("# ... and", length(x) - 10, "more\n")
  }
  invisible(x)
}


# make_measure_function ----

#' Create a measure function with automatic dispatch
#'
#' Higher-order function factory that creates measure_ functions with automatic
#' dispatch to handle single matrices, lists of matrices, and tibbles.
#'
#' @param impl_fn Function. The implementation function that operates on a single
#'   matrix (nx2) and returns a scalar. Should accept `x` as first argument and
#'   `...` for additional args.
#' @param fn_name Character. Optional name for debugging (not used functionally).
#'
#' @return A function that dispatches based on input type.
#'
#' @details
#' The returned function automatically:
#' - Applies impl_fn to single matrices and returns a scalar
#' - Applies impl_fn to each element of a list and returns a numeric vector with class "meas"
#' - Applies impl_fn to specified columns of a tibble and adds new column(s)
#'
#' For tibbles, the `.name` argument controls the output column name:
#' - If `.name` is provided: uses that name
#' - If `.name` is NULL: uses "colname_measurename" (e.g., "coo_area")
#'
#' Additional arguments are passed through via `...`
#'
#' @keywords internal
#' @noRd
make_measure_function <- function(impl_fn, fn_name = NULL) {
  # Infer function name if not provided
  if (is.null(fn_name)) {
    fn_name <- deparse(substitute(impl_fn))
  }

  f <- function(x, ..., .cols = NULL, .name = NULL) {
    # Single matrix case - return scalar
    if (is.matrix(x)) {
      return(impl_fn(x, ...))
    }

    # List case - return numeric vector with meas class
    if (is.list(x) && !is.data.frame(x)) {
      result <- vapply(x, impl_fn, numeric(1), ...)
      return(as_meas(result))
    }

    # Tibble case - add new column(s)
    if (is.data.frame(x)) {
      # Capture .cols with tidyeval
      .cols_quo <- rlang::enquo(.cols)
      if (rlang::quo_is_null(.cols_quo)) {
        cols_to_process <- get_coo_cols(x, NULL)
      } else {
        cols_idx <- tidyselect::eval_select(.cols_quo, x)
        cols_to_process <- names(cols_idx)
      }

      # Extract measure name from function name (e.g., ".measure_area" -> "area")
      measure_name <- gsub("^\\.measure_", "", fn_name)

      # Compute measures for each column
      for (col in cols_to_process) {
        # Determine new column name
        if (is.null(.name)) {
          new_col_name <- paste0(col, "_", measure_name)
        } else {
          new_col_name <- .name
        }

        # Compute measure for this column and add as new column
        x[[new_col_name]] <- vapply(x[[col]], impl_fn, numeric(1), ...)
      }

      return(x)
    }
  }

  # Store metadata for display
  attr(f, "impl_fn") <- fn_name

  # Custom class for display
  class(f) <- c("momocs2_measure_function", "function")

  return(f)
}


# print.momocs2_measure_function ----

#' @export
print.momocs2_measure_function <- function(x, ...) {
  impl_fn <- attr(x, "impl_fn")
  cat(sprintf("# Momocs2 measure function wrapping: %s\n", impl_fn))
  cat(sprintf("# View implementation: Momocs2:::%s\n\n", impl_fn))
  NextMethod()
}


# measure_area ----

#' Measure area of a closed outline
#'
#' Calculate the area enclosed by a shape using the shoelace formula.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#' @param .name Character. Name for the new column when `x` is a tibble. If `NULL`,
#'   uses "colname_area" (e.g., "coo_area").
#'
#' @return
#' * If `x` is a single matrix: returns a numeric scalar
#' * If `x` is a list: returns a numeric vector of class "meas"
#' * If `x` is a tibble: returns the tibble with new area column(s) added
#'
#' @details
#' Uses the shoelace formula (also called surveyor's formula) to compute area.
#' The outline is automatically treated as closed (first point connects to last).
#'
#' @examples
#' measure_area(shapes$cat)
#' measure_area(shapes)
#' measure_area(bot)
#' measure_area(bot, .name = "area")
#' measure_area(bot, .cols = "coo", .name = "my_area")
#'
#' @seealso [measure_perim()] for perimeter; [measure_centroid_size()] for centroid size
#'
#' @keywords internal
#' @export
measure_area <- make_measure_function(.measure_area)

.measure_area <- function(x, ...) {
  if (!is.matrix(x)) return(NA_real_)
  n <- nrow(x)
  # Shoelace formula - vectorized
  x1 <- x[, 1]
  y1 <- x[, 2]
  x2 <- c(x1[-1], x1[1])  # shift x coordinates (wrap around)
  y2 <- c(y1[-1], y1[1])  # shift y coordinates (wrap around)
  abs(0.5 * sum(x1 * y2 - x2 * y1))
}


# measure_perim ----

#' Measure perimeter of an outline
#'
#' Calculate the total perimeter (length) of a shape.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#' @param .name Character. Name for the new column when `x` is a tibble. If `NULL`,
#'   uses "colname_perim" (e.g., "coo_perim").
#'
#' @return
#' * If `x` is a single matrix: returns a numeric scalar
#' * If `x` is a list: returns a numeric vector of class "meas"
#' * If `x` is a tibble: returns the tibble with new perimeter column(s) added
#'
#' @details
#' Computes the sum of Euclidean distances between consecutive points,
#' including the distance from the last point back to the first.
#'
#' @examples
#' measure_perim(shapes$cat)
#' measure_perim(shapes)
#' measure_perim(bot)
#' measure_perim(bot, .name = "perimeter")
#'
#' @seealso [measure_area()] for area; [get_perim()] for the getter function
#'
#' @keywords internal
#' @export
measure_perim <- make_measure_function(.measure_perim)

.measure_perim <- function(x, ...) {
  .get_perim(x)
}


# measure_centroid_size ----

#' Measure centroid size
#'
#' Calculate the centroid size: the square root of the sum of squared
#' distances from each point to the centroid.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#' @param .name Character. Name for the new column when `x` is a tibble. If `NULL`,
#'   uses "colname_centroid_size" (e.g., "coo_centroid_size").
#'
#' @return
#' * If `x` is a single matrix: returns a numeric scalar
#' * If `x` is a list: returns a numeric vector of class "meas"
#' * If `x` is a tibble: returns the tibble with new centroid size column(s) added
#'
#' @details
#' Centroid size is a common size measure in geometric morphometrics.
#' It is scale-independent and used for allometric correction.
#'
#' @examples
#' measure_centroid_size(shapes$cat)
#' measure_centroid_size(shapes)
#' measure_centroid_size(bot)
#' measure_centroid_size(bot, .name = "CS")
#'
#' @seealso [measure_centroid_size_norm()] for normalized centroid size; [get_centroid_size()] for the getter function
#'
#' @keywords internal
#' @export
measure_centroid_size <- make_measure_function(.measure_centroid_size)

.measure_centroid_size <- function(x, ...) {
  .get_centroid_size(x)
}


# measure_centroid_size_norm ----

#' Measure normalized centroid size
#'
#' Calculate the normalized centroid size (centroid size divided by perimeter).
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#' @param .name Character. Name for the new column when `x` is a tibble. If `NULL`,
#'   uses "colname_centroid_size_norm" (e.g., "coo_centroid_size_norm").
#'
#' @return
#' * If `x` is a single matrix: returns a numeric scalar
#' * If `x` is a list: returns a numeric vector of class "meas"
#' * If `x` is a tibble: returns the tibble with new normalized centroid size column(s) added
#'
#' @details
#' Normalized centroid size provides a scale-independent measure that accounts
#' for both size (centroid size) and shape complexity (perimeter).
#'
#' @examples
#' measure_centroid_size_norm(shapes$cat)
#' measure_centroid_size_norm(shapes)
#' measure_centroid_size_norm(bot)
#' measure_centroid_size_norm(bot, .name = "CS_norm")
#'
#' @seealso [measure_centroid_size()] for centroid size; [get_centroid_size_norm()] for the getter function
#'
#' @keywords internal
#' @export
measure_centroid_size_norm <- make_measure_function(.measure_centroid_size_norm)

.measure_centroid_size_norm <- function(x, ...) {
  .get_centroid_size_norm(x)
}
