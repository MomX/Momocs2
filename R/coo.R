# make_coo_function ----

#' Create a coo function with automatic dispatch
#'
#' Higher-order function factory that creates coo_ functions with automatic
#' dispatch to handle single matrices, lists of matrices, and tibbles.
#'
#' @param impl_fn Function. The implementation function that operates on a single
#'   matrix (nx2). Should accept `x` as first argument and `...` for additional args.
#' @param fn_name Character. Optional name for debugging (not used functionally).
#' @param sync_ldk Logical. If TRUE, function is landmark-aware and will sync
#'   landmark columns automatically. Default is FALSE.
#'
#' @return A function that dispatches based on input type.
#'
#' @details
#' The returned function automatically:
#' - Applies impl_fn to single matrices and returns the result
#' - Applies impl_fn to each element of a list and returns a list
#' - Applies impl_fn to specified columns of a tibble and returns the modified tibble
#'
#' When `sync_ldk = TRUE`, the dispatcher automatically handles landmark columns:
#' - Looks for a column named `colname_ldk` (e.g., `coo_ldk` for `coo` column)
#' - Or uses `.ldk_col` argument if provided
#' - Passes landmarks to implementation function
#' - Updates landmarks based on coordinate changes
#'
#' Additional arguments are passed through via `...`
#'
#' @keywords internal
#' @noRd
make_coo_function <- function(impl_fn, fn_name = NULL, sync_ldk = FALSE) {
  # Infer function name if not provided
  if (is.null(fn_name)) {
    fn_name <- deparse(substitute(impl_fn))
  }

  f <- function(x, ..., .cols = NULL, .ldk_col = NULL) {
    if (is.data.frame(x)) {
      # Capture .cols with tidyeval
      .cols_quo <- rlang::enquo(.cols)
      if (rlang::quo_is_null(.cols_quo)) {
        cols_to_process <- get_coo_cols(x, NULL)
      } else {
        cols_idx <- tidyselect::eval_select(.cols_quo, x)
        cols_to_process <- names(cols_idx)
      }

      for (col in cols_to_process) {
        if (sync_ldk) {
          # Determine landmark column name with tidyeval support
          .ldk_col_quo <- rlang::enquo(.ldk_col)
          ldk_col_name <- if (!rlang::quo_is_null(.ldk_col_quo)) {
            rlang::as_name(.ldk_col_quo)
          } else {
            paste0(col, "_ldk")
          }

          if (ldk_col_name %in% names(x)) {
            # Process with landmarks
            results <- Map(function(coo, ldk) {
              impl_fn(coo, ldk = ldk, ...)
            }, x[[col]], x[[ldk_col_name]])

            # Extract results
            x[[col]] <- lapply(results, `[[`, "coo")
            x[[ldk_col_name]] <- lapply(results, `[[`, "ldk")

            message(sprintf("\u2714 Updated landmarks in '%s'", ldk_col_name))
          } else {
            # No landmarks found, process normally
            results <- lapply(x[[col]], function(coo) {
              impl_fn(coo, ldk = NULL, ...)
            })
            x[[col]] <- lapply(results, `[[`, "coo")
          }
        } else {
          # No landmark support needed
          x[[col]] <- lapply(x[[col]], impl_fn, ...)
        }
      }
      return(x)
    }

    if (is.list(x)) {
      if (sync_ldk) {
        results <- lapply(x, function(coo) impl_fn(coo, ldk = NULL, ...))
        lapply(results, `[[`, "coo")
      } else {
        lapply(x, impl_fn, ...)
      }
    } else {
      if (sync_ldk) {
        impl_fn(x, ldk = NULL, ...)$coo
      } else {
        impl_fn(x, ...)
      }
    }
  }

  # Store metadata for display
  attr(f, "impl_fn") <- fn_name
  attr(f, "sync_ldk") <- sync_ldk

  # Custom class for display
  class(f) <- c("momocs2_function", "function")

  return(f)
}


# print.momocs2_function ----

#' @export
print.momocs2_function <- function(x, ...) {
  impl_fn <- attr(x, "impl_fn")
  sync_ldk <- attr(x, "sync_ldk")
  cat(sprintf("# Momocs2 function wrapping: %s\n", impl_fn))
  if (sync_ldk) {
    cat("# Landmark-aware: yes\n")
  }
  cat(sprintf("# View implementation: Momocs2:::%s\n\n", impl_fn))
  NextMethod()
}


# get_coo_cols ----

#' Identify coo columns in a tibble
#'
#' Detect which columns in a tibble/data.frame contain coo objects
#' or list columns of matrices.
#'
#' @param df A tibble or data.frame.
#' @param .cols Character vector or NULL. If specified, use these column names.
#'   If NULL, auto-detect columns containing coo objects or matrices.
#'
#' @return Character vector of column names to process.
#'
#' @details
#' Detection priority:
#' 1. Columns with class "coo" (list of matrices with coo class)
#' 2. List columns where all elements are matrices
#'
#' When multiple qualifying columns exist, an error is raised and user must
#' specify `.cols` explicitly.
#'
#' @examples
#' get_coo_cols(bot)
#' get_coo_cols(bot, "coo")
#'
#' @keywords internal
#' @export
get_coo_cols <- function(df, .cols = NULL) {
  if (!is.null(.cols)) {
    # User explicitly specified which columns (support tidyselect via enquo/quo_name)
    if (is.character(.cols)) {
      # Validate columns exist
      missing_cols <- setdiff(.cols, names(df))
      if (length(missing_cols) > 0) {
        stop(sprintf("Column(s) not found: %s",
                     paste(missing_cols, collapse = ", ")))
      }
      return(.cols)
    } else {
      # Could be logical or integer index
      return(.cols)
    }
  }

  # Helper to check if column contains matrices
  is_matrix_list <- function(col) {
    if (!is.list(col) || length(col) == 0) return(FALSE)
    # Check if all elements are matrices
    all(sapply(col, is.matrix))
  }

  # First, look for columns with class "coo" (list of matrices with coo class)
  coo_cols <- names(df)[sapply(df, function(col) {
    "coo" %in% class(col)
  })]

  # If found, check if multiple
  if (length(coo_cols) > 1) {
    stop(sprintf("Multiple coo columns found: %s. Specify '.cols' to choose which to process.",
                 paste(coo_cols, collapse = ", ")))
  }

  if (length(coo_cols) == 1) {
    return(coo_cols)
  }

  # Otherwise, look for list columns of matrices (any list, named or not)
  coo_cols <- names(df)[sapply(df, is_matrix_list)]

  # Check if multiple list columns of matrices
  if (length(coo_cols) > 1) {
    stop(sprintf("Multiple list columns of matrices found: %s. Specify '.cols' to choose which to process.",
                 paste(coo_cols, collapse = ", ")))
  }

  if (length(coo_cols) == 0) {
    stop("No columns of class 'coo' or list columns of matrices found. Specify '.cols' argument explicitly.")
  }

  coo_cols
}


# coo_extract ----

#' Extract or subset coordinates
#'
#' Subset or trim shapes by row index or position.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param id Integer vector of row indices to extract (for `coo_extract()`).
#' @param n Integer. Number of points to keep or remove.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#' @param .ldk_col Character. Name of landmark column. If `NULL`, uses `colname_ldk`.
#'
#' @details
#' - `coo_extract()`: extract specific rows by index
#' - `coo_head()`: keep first n points
#' - `coo_tail()`: keep all but the last n points
#'
#' If `n` equals `nrow(x)`, the shape is returned unchanged.
#'
#' These functions are landmark-aware: invalid landmark indices are automatically
#' filtered out after subsetting.
#'
#' @return
#' * If `x` is a single matrix: returns a matrix with selected/trimmed rows
#' * If `x` is a list: returns a list of matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns subsetted
#'
#' @examples
#' coo_extract(shapes$cat, c(1, 10, 50))
#' coo_extract(shapes)
#' coo_head(shapes$cat, 10)
#' coo_head(shapes)
#' coo_tail(shapes$cat, 5)
#' coo_head(bot, 20)
#'
#' @seealso [coo_sample()] for resampling to different point counts
#'
#' @name coo_extract
#' @keywords internal
NULL

#' @rdname coo_extract
#' @export
coo_extract <- make_coo_function(.coo_extract, sync_ldk = TRUE)

#' @rdname coo_extract
#' @export
coo_head <- make_coo_function(.coo_head, sync_ldk = TRUE)

#' @rdname coo_extract
#' @export
coo_tail <- make_coo_function(.coo_tail, sync_ldk = TRUE)

.coo_extract <- function(x, ldk = NULL, id, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))

  new_coo <- x[id, , drop = FALSE]

  # Filter landmarks - keep only valid indices in new coordinate system
  if (!is.null(ldk) && length(ldk) > 0) {
    # Map old indices to new indices
    new_ldk <- which(id %in% ldk)
  } else {
    new_ldk <- ldk
  }

  list(coo = new_coo, ldk = new_ldk)
}

.coo_head <- function(x, ldk = NULL, n=10, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))
  if (n < 1) stop("n must be at least 1")
  if (n == nrow(x)) return(list(coo = x, ldk = ldk))

  new_coo <- x[1:n, , drop = FALSE]

  # Filter landmarks
  if (!is.null(ldk) && length(ldk) > 0) {
    new_ldk <- ldk[ldk >= 1 & ldk <= n]
  } else {
    new_ldk <- ldk
  }

  list(coo = new_coo, ldk = new_ldk)
}

.coo_tail <- function(x, ldk = NULL, n=10, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))
  if (n < 0) stop("n must be >= 0")
  if (n == 0) return(list(coo = x, ldk = ldk))
  if (n >= nrow(x)) stop(sprintf("n (%d) must be less than nrow(x) (%d)", n, nrow(x)))

  new_coo <- x[1:(nrow(x) - n), , drop = FALSE]
  new_n <- nrow(new_coo)

  # Filter landmarks
  if (!is.null(ldk) && length(ldk) > 0) {
    new_ldk <- ldk[ldk >= 1 & ldk <= new_n]
  } else {
    new_ldk <- ldk
  }

  list(coo = new_coo, ldk = new_ldk)
}


# coo_center ----

#' Center coordinates
#'
#' Center a shape around the origin using centering (subtract the mean).
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns the centered matrix
#' * If `x` is a list: returns a list of centered matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns centered
#'
#' @examples
#' coo_center(shapes$cat)
#' coo_center(shapes)
#' coo_center(bot)
#'
#' @seealso [coo_center_to()] for centering and translating; [coo_translate()] for translation only; [get_centroid()] for centroid coordinates
#'
#' @keywords internal
#' @export
coo_center <- make_coo_function(.coo_center)

.coo_center <- function(x, ...) {
  if (!is.matrix(x)) return(x)
  scale(x, center = TRUE, scale = FALSE)
}


# coo_center_to ----

#' Center and translate coordinates
#'
#' Center a shape and then translate it to specified coordinates.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param x_val Numeric. X coordinate to translate to. Default is 0.
#' @param y_val Numeric. Y coordinate to translate to. Default is 0.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns the centered and translated matrix
#' * If `x` is a list: returns a list of centered and translated matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns transformed
#'
#' @keywords internal
#' @export
coo_center_to <- make_coo_function(.coo_center_to)

.coo_center_to <- function(x, x_val = 0, y_val = 0, ...) {
  if (!is.matrix(x)) return(x)
  x %>% .coo_center() %>% .coo_translate(x_val = x_val, y_val = y_val)
}


# coo_translate ----

#' Translate coordinates
#'
#' Move a shape by a specified distance in x and y directions.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param x_val Numeric. Distance to translate in x direction. Default is 0.
#' @param y_val Numeric. Distance to translate in y direction. Default is 0.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns the translated matrix
#' * If `x` is a list: returns a list of translated matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns translated
#'
#' @examples
#' coo_translate(shapes$cat, x_val = 10, y_val = 20)
#' coo_translate(shapes, x_val = 10)
#' coo_translate(bot, x_val = 50, y_val = 50)
#'
#' @seealso [coo_center()] for centering; [coo_center_to()] for combined centering and translation
#'
#' @keywords internal
#' @export
coo_translate <- make_coo_function(.coo_translate)

.coo_translate <- function(x, x_val = 0, y_val = 0, ...) {
  if (!is.matrix(x)) return(x)
  cbind(x[, 1] + x_val, x[, 2] + y_val)
}


# coo_translate_to_xaxis ----

#' Translate shape to x-axis
#'
#' Translate a shape so its centroid aligns with the x-axis.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns the transformed matrix
#' * If `x` is a list: returns a list of transformed matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns transformed
#'
#' @keywords internal
#' @export
coo_translate_to_xaxis <- make_coo_function(.coo_translate_to_xaxis)

.coo_translate_to_xaxis <- function(x, ...) {
  if (!is.matrix(x)) return(x)
  x %>% .coo_translate(y_val = -.get_centroid(x)[2])
}


# coo_translate_to_yaxis ----

#' Translate shape to y-axis
#'
#' Translate a shape so its centroid aligns with the y-axis.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns the transformed matrix
#' * If `x` is a list: returns a list of transformed matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns transformed
#'
#' @keywords internal
#' @export
coo_translate_to_yaxis <- make_coo_function(.coo_translate_to_yaxis)

.coo_translate_to_yaxis <- function(x, ...) {
  if (!is.matrix(x)) return(x)
  x %>% .coo_translate(x_val = -.get_centroid(x)[1])
}


# coo_translate_jitter ----

#' Jitter coordinates
#'
#' Add random translation to a shape.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param amount Numeric. Maximum jitter amount. Default is 10% of normalized centroid size.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns the jittered matrix
#' * If `x` is a list: returns a list of jittered matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns jittered
#'
#' @seealso [coo_translate()] for deterministic translation
#'
#' @keywords internal
#' @importFrom stats runif
#' @export
coo_translate_jitter <- make_coo_function(.coo_translate_jitter)

.coo_translate_jitter <- function(x, amount = 0.1, ...) {
  if (!is.matrix(x)) return(x)
  amount <- 0.1 * get_centroid_size_norm(x)
  x %>% .coo_translate(x_val = runif(1, -amount, amount),
                       y_val = runif(1, -amount, amount))
}


# coo_affine ----

#' Apply affine transformation
#'
#' Apply an affine transformation (rotation, scaling, shearing, etc.) to a shape
#' using a 2x2 transformation matrix.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param mat A 2x2 numeric matrix defining the affine transformation.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns the transformed matrix
#' * If `x` is a list: returns a list of transformed matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns transformed
#'
#' @details
#' The transformation is applied as: `x_new = x %*% mat`
#'
#' Common transformation matrices:
#' * Rotation by angle θ: `matrix(c(cos(θ), sin(θ), -sin(θ), cos(θ)), nrow = 2)`
#' * Scaling by sx, sy: `matrix(c(sx, 0, 0, sy), nrow = 2)`
#' * Shear in x: `matrix(c(1, 0, k, 1), nrow = 2)`
#'
#' @examples
#' # Rotation matrix
#' rot_mat <- matrix(c(cos(pi/4), sin(pi/4), -sin(pi/4), cos(pi/4)), nrow = 2)
#' coo_affine(shapes$cat, rot_mat)
#'
#' # Scaling matrix
#' scale_mat <- matrix(c(2, 0, 0, 2), nrow = 2)
#' coo_affine(shapes, scale_mat)
#'
#' @seealso [coo_rotate()] for rotation; [coo_shear()] for shearing; [coo_flip()] for flipping
#'
#' @keywords internal
#' @export
coo_affine <- make_coo_function(.coo_affine)

.coo_affine <- function(x, mat, ...) {
  if (!is.matrix(x)) return(x)
  x %*% mat
}


# coo_rotate ----

#' Rotate coordinates
#'
#' Rotate a shape by a specified angle around the origin.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param theta Numeric. Rotation angle in radians. Default is 0.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns the rotated matrix
#' * If `x` is a list: returns a list of rotated matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns rotated
#'
#' @examples
#' coo_rotate(shapes$cat, theta = pi / 4)
#' coo_rotate(shapes, theta = pi / 6)
#' coo_rotate(bot, theta = pi / 8)
#'
#' @seealso [coo_rotate_around()] for rotation around a specific point; [coo_slide_angle()] for rotating point order by angle
#'
#' @keywords internal
#' @export
coo_rotate <- make_coo_function(.coo_rotate)

.coo_rotate <- function(x, theta = 0, ...) {
  if (!is.matrix(x)) return(x)
  aff <- matrix(c(cos(-theta), sin(-theta),
                  -sin(-theta), cos(-theta)),
                nrow = 2)
  x %*% aff
}


# coo_rotate_around ----

#' Rotate around a center point
#'
#' Rotate a shape by a specified angle around a given center point.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param theta Numeric. Rotation angle in radians. Default is 0.
#' @param center Numeric vector of length 2 (x, y). Center point for rotation.
#'   Default is the centroid.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns the rotated matrix
#' * If `x` is a list: returns a list of rotated matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns rotated
#'
#' @keywords internal
#' @export
coo_rotate_around <- make_coo_function(.coo_rotate_around)

.coo_rotate_around <- function(x, theta = 0, center = NULL, ...) {
  if (!is.matrix(x)) return(x)
  if (is.null(center)) {
    center <- .get_centroid(x)
  }
  x %>%
    sweep(2, center, "-") %>%
    .coo_rotate(theta) %>%
    sweep(2, center, "+")
}


# coo_align ----

#' Align shape to principal axes
#'
#' Align a shape to its principal axes (major and minor axes) and recenter.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns the aligned matrix
#' * If `x` is a list: returns a list of aligned matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns aligned
#'
#' @details
#' Aligns to principal axes using SVD of the covariance matrix and recenters
#' to the original centroid position.
#'
#' @importFrom stats var
#' @keywords internal
#' @export
coo_align <- make_coo_function(.coo_align)

.coo_align <- function(x, ...) {
  if (!is.matrix(x)) return(x)
  old_cent <- .get_centroid(x)
  coo_aligned <- x %*% svd(var(x))$u
  new_cent <- .get_centroid(coo_aligned)
  xy <- old_cent - new_cent
  coo_aligned %>% .coo_translate(x_val = xy[1], y_val = xy[2])
}


# coo_align_minor ----

#' Align shape to minor and major axes (swapped)
#'
#' Align a shape to its principal axes with major and minor axes swapped.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns the aligned matrix
#' * If `x` is a list: returns a list of aligned matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns aligned
#'
#' @keywords internal
#' @export
coo_align_minor <- make_coo_function(.coo_align_minor)

.coo_align_minor <- function(x, ...) {
  if (!is.matrix(x)) return(x)
  old_cent <- .get_centroid(x)
  coo_aligned <- x %*% svd(var(x))$u[, c(2, 1)]
  new_cent <- .get_centroid(coo_aligned)
  xy <- old_cent - new_cent
  coo_aligned %>% .coo_translate(x_val = xy[1], y_val = xy[2])
}


# coo_flip ----

#' Flip coordinates
#'
#' Flip a shape along a specified axis or line.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param xintercept Numeric. X intercept for flipping. Used in `coo_flip_x()`.
#' @param yintercept Numeric. Y intercept for flipping. Used in `coo_flip_y()`.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @details
#' * `coo_flip_x()`: flip across vertical line at x intercept
#' * `coo_flip_xaxis()`: flip across x-axis (y = 0)
#' * `coo_flip_y()`: flip across horizontal line at y intercept
#' * `coo_flip_yaxis()`: flip across y-axis (x = 0)
#'
#' @return
#' * If `x` is a single matrix: returns the flipped matrix
#' * If `x` is a list: returns a list of flipped matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns flipped
#'
#' @seealso [coo_rotate()] for rotation; [coo_align()] for alignment
#'
#' @name coo_flip
#' @keywords internal
NULL

#' @rdname coo_flip
#' @export
coo_flip_x <- make_coo_function(.coo_flip_x)

#' @rdname coo_flip
#' @export
coo_flip_xaxis <- make_coo_function(.coo_flip_xaxis)

#' @rdname coo_flip
#' @export
coo_flip_y <- make_coo_function(.coo_flip_y)

#' @rdname coo_flip
#' @export
coo_flip_yaxis <- make_coo_function(.coo_flip_yaxis)

.coo_flip_x <- function(x, xintercept = NULL, ...) {
  if (!is.matrix(x)) return(x)
  if (is.null(xintercept)) xintercept <- mean(x[, 1])
  x[, 1] <- 2 * xintercept - x[, 1]
  x
}

.coo_flip_xaxis <- function(x, ...) {
  if (!is.matrix(x)) return(x)
  aff <- matrix(c(1, 0, 0, -1), nrow = 2)
  x %*% aff
}

.coo_flip_y <- function(x, yintercept = NULL, ...) {
  if (!is.matrix(x)) return(x)
  if (is.null(yintercept)) yintercept <- mean(x[, 2])
  x[, 2] <- 2 * yintercept - x[, 2]
  x
}

.coo_flip_yaxis <- function(x, ...) {
  if (!is.matrix(x)) return(x)
  aff <- matrix(c(-1, 0, 0, 1), nrow = 2)
  x %*% aff
}


# coo_shear ----

#' Shear coordinates
#'
#' Apply shear transformation to a shape.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param k Numeric. Shear factor. Default is 1.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @details
#' * `coo_shear_x()`: shear in x direction
#' * `coo_shear_y()`: shear in y direction
#'
#' @return
#' * If `x` is a single matrix: returns the sheared matrix
#' * If `x` is a list: returns a list of sheared matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns sheared
#'
#' @seealso [coo_rotate()] for rotation; [coo_flip()] for flipping
#'
#' @name coo_shear
#' @keywords internal
NULL

#' @rdname coo_shear
#' @export
coo_shear_x <- make_coo_function(.coo_shear_x)

#' @rdname coo_shear
#' @export
coo_shear_y <- make_coo_function(.coo_shear_y)

.coo_shear_x <- function(x, k = 1, ...) {
  if (!is.matrix(x)) return(x)
  aff <- matrix(c(1, 0, k, 1), nrow = 2)
  x %*% aff
}

.coo_shear_y <- function(x, k = 1, ...) {
  if (!is.matrix(x)) return(x)
  aff <- matrix(c(1, k, 0, 1), nrow = 2)
  x %*% aff
}


# coo_slide ----

#' Slide coordinates along outline
#'
#' Rotate the order of points along the outline without changing the shape.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param id Integer. Index of point to become the first point. Default is 1.
#' @param target Numeric vector of length 2 (x, y). Find closest point to target.
#' @param theta Numeric. Angle in radians. Find point closest to this angle.
#' @param direction Character. Direction ("right", "up", "left", "down").
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#' @param .ldk_col Character. Name of landmark column. If `NULL`, uses `colname_ldk`.
#'
#' @details
#' - `coo_slide()`: slide to specific index
#' - `coo_slide_closest()`: slide to point closest to spatial target
#' - `coo_slide_angle()`: slide to point closest to angle direction
#' - `coo_slide_direction()`: slide to point in specified direction
#' - `coo_slide_gap()`: slide to largest gap in perimeter
#'
#' These functions are landmark-aware: landmark indices are automatically
#' shifted (with wrapping) to match the new point order.
#'
#' @return
#' * If `x` is a single matrix: returns the matrix with reordered points
#' * If `x` is a list: returns a list of matrices with reordered points
#' * If `x` is a tibble: returns the tibble with specified coo columns reordered
#'
#' @keywords internal
#' @export
coo_slide <- make_coo_function(.coo_slide, sync_ldk = TRUE)

#' @rdname coo_slide
#' @export
coo_slide_closest <- make_coo_function(.coo_slide_closest, sync_ldk = TRUE)

#' @rdname coo_slide
#' @export
coo_slide_angle <- make_coo_function(.coo_slide_angle, sync_ldk = TRUE)

#' @rdname coo_slide
#' @export
coo_slide_direction <- make_coo_function(.coo_slide_direction, sync_ldk = TRUE)

#' @rdname coo_slide
#' @export
coo_slide_gap <- make_coo_function(.coo_slide_gap, sync_ldk = TRUE)

.coo_slide <- function(x, ldk = NULL, id = 1, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))
  if (id == 1) return(list(coo = x, ldk = ldk))

  n <- nrow(x)
  if (id > n || id < 1) stop("id must be between 1 and n")

  # Slide coordinates
  new_ids <- c(id:n, 1:(id - 1))
  new_coo <- x[new_ids, ]

  # Update landmarks by shifting indices with wrapping
  if (!is.null(ldk) && length(ldk) > 0) {
    shift <- id - 1
    new_ldk <- ((ldk - shift - 1) %% n) + 1
  } else {
    new_ldk <- ldk
  }

  list(coo = new_coo, ldk = new_ldk)
}

.coo_slide_closest <- function(x, ldk = NULL, target, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))
  closest_id <- .get_closest(x, target)
  .coo_slide(x, ldk = ldk, id = closest_id)
}

.coo_slide_angle <- function(x, ldk = NULL, theta = 0, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))
  closest_id <- .get_closest_angle(x, theta = theta)
  .coo_slide(x, ldk = ldk, id = closest_id)
}

.coo_slide_direction <- function(x, ldk = NULL, direction = c("right", "up", "left", "down"), ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))
  direction <- match.arg(direction)
  theta <- switch(direction,
                  "right" = 0,
                  "up" = pi/2,
                  "left" = pi,
                  "down" = -pi/2)
  .coo_slide_angle(x, ldk = ldk, theta = theta)
}

.coo_slide_gap <- function(x, ldk = NULL, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))
  gap_id <- .get_perim_along(x) %>% which.max()
  .coo_slide(x, ldk = ldk, id = gap_id + 1)
}


# coo_reverse ----

#' Reverse point order
#'
#' Reverse the order of points along an outline.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#' @param .ldk_col Character. Name of landmark column. If `NULL`, uses `colname_ldk`.
#'
#' @return
#' * If `x` is a single matrix: returns the matrix with reversed point order
#' * If `x` is a list: returns a list of matrices with reversed point order
#' * If `x` is a tibble: returns the tibble with specified coo columns reversed
#'
#' @details
#' This function is landmark-aware: landmark indices are automatically
#' reversed to match the new point order using the formula `n - ldk + 1`.
#'
#' @examples
#' coo_reverse(shapes$cat)
#' coo_reverse(shapes)
#' coo_reverse(bot)
#'
#' @keywords internal
#' @export
coo_reverse <- make_coo_function(.coo_reverse, sync_ldk = TRUE)

.coo_reverse <- function(x, ldk = NULL, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))

  n <- nrow(x)
  new_coo <- x[n:1, ]

  # Reverse landmarks
  if (!is.null(ldk) && length(ldk) > 0) {
    new_ldk <- n - ldk + 1
  } else {
    new_ldk <- ldk
  }

  list(coo = new_coo, ldk = new_ldk)
}


# coo_sample ----

#' Sample coordinates
#'
#' Resample a shape to a different number of points.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param n Integer. Target number of points. Minimum is 3.
#' @param prop Numeric. Proportion of points to keep (0 to 1).
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#' @param .ldk_col Character. Name of landmark column. If `NULL`, uses `colname_ldk`.
#'
#' @details
#' * `coo_sample()`: resample to exactly n points via arc-length interpolation
#' * `coo_sample_prop()`: resample to a proportion of original points
#'
#' These functions are landmark-aware: if landmarks are present, the outline
#' is resampled segment-by-segment between landmarks, preserving landmark
#' positions and distributing points proportionally across segments.
#'
#' @return
#' * If `x` is a single matrix: returns the resampled matrix
#' * If `x` is a list: returns a list of resampled matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns resampled
#'
#' @seealso [coo_smooth()] for smoothing
#'
#' @name coo_sample
#' @keywords internal
#' @importFrom stats approx
NULL

#' @rdname coo_sample
#' @export
coo_sample <- make_coo_function(.coo_sample, sync_ldk = TRUE)

#' @rdname coo_sample
#' @export
coo_sample_prop <- make_coo_function(.coo_sample_prop, sync_ldk = TRUE)

.coo_sample <- function(x, ldk = NULL, n, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))
  if (n < 3) stop("n must be at least 3")
  if (n == nrow(x)) return(list(coo = x, ldk = ldk))

  # Regular sampling (no landmarks)
  if (is.null(ldk) || length(ldk) == 0) {
    dists <- sqrt(rowSums(diff(rbind(x, x[1,]))^2))
    cum_dists <- c(0, cumsum(dists))
    perim <- cum_dists[length(cum_dists)]
    target_dists <- seq(0, perim * (n-1)/n, length = n)

    res <- matrix(NA, n, 2)
    res[, 1] <- approx(cum_dists, c(x[, 1], x[1, 1]), xout = target_dists, rule = 2)$y
    res[, 2] <- approx(cum_dists, c(x[, 2], x[1, 2]), xout = target_dists, rule = 2)$y

    return(list(coo = res, ldk = NULL))
  }

  # Landmark-aware sampling: preserve exact landmarks, distribute points between them
  ldk_sorted <- sort(unique(ldk))
  n_ldk <- length(ldk_sorted)

  # Points to distribute between landmarks (excluding the landmarks themselves)
  n_between <- n - n_ldk
  if (n_between < 0) stop("n must be at least equal to number of landmarks")

  # Calculate perimeter of each segment to allocate points proportionally
  segment_perims <- numeric(n_ldk)
  segments_coords <- vector("list", n_ldk)

  for (i in seq_len(n_ldk)) {
    start_idx <- ldk_sorted[i]
    end_idx <- if (i == n_ldk) ldk_sorted[1] else ldk_sorted[i + 1]

    # Extract segment (from landmark i to landmark i+1)
    if (end_idx > start_idx) {
      segment <- x[start_idx:end_idx, , drop = FALSE]
    } else {
      # Wrap around
      segment <- rbind(x[start_idx:nrow(x), ], x[1:end_idx, ])
    }

    segments_coords[[i]] <- segment
    # Calculate segment perimeter
    segment_perims[i] <- sum(sqrt(rowSums(diff(segment)^2)))
  }

  # Allocate points proportionally by perimeter
  total_perim <- sum(segment_perims)
  points_per_seg <- round(n_between * segment_perims / total_perim)

  # Ensure exactly n_between points are allocated
  while (sum(points_per_seg) != n_between) {
    diff_pts <- n_between - sum(points_per_seg)
    if (diff_pts > 0) {
      # Add to segment with largest remainder
      remainders <- (n_between * segment_perims / total_perim) - points_per_seg
      points_per_seg[which.max(remainders)] <- points_per_seg[which.max(remainders)] + 1
    } else {
      # Remove from segment with smallest remainder
      remainders <- (n_between * segment_perims / total_perim) - points_per_seg
      points_per_seg[which.min(remainders)] <- points_per_seg[which.min(remainders)] - 1
    }
  }

  # Resample each segment
  result_segments <- vector("list", n_ldk)
  new_ldk <- integer(n_ldk)
  current_idx <- 1

  for (i in seq_len(n_ldk)) {
    segment <- segments_coords[[i]]
    n_between_this <- points_per_seg[i]  # points between landmarks
    seg_n_total <- n_between_this + 1  # +1 for the starting landmark

    if (seg_n_total == 1) {
      # Just the landmark itself
      seg_sampled <- segment[1, , drop = FALSE]
    } else {
      # Calculate cumulative distances
      dists <- sqrt(rowSums(diff(segment)^2))
      cum_dists <- c(0, cumsum(dists))
      seg_perim <- cum_dists[length(cum_dists)]

      # Sample seg_n_total points, but exclude endpoint to avoid duplication with next segment
      # Range from 0 to just before the end
      target_dists <- seq(0, seg_perim * (seg_n_total - 1) / seg_n_total, length.out = seg_n_total)

      seg_sampled <- matrix(NA, seg_n_total, 2)
      seg_sampled[, 1] <- approx(cum_dists, segment[, 1], xout = target_dists, rule = 2)$y
      seg_sampled[, 2] <- approx(cum_dists, segment[, 2], xout = target_dists, rule = 2)$y

      # Force first point to be EXACT landmark coordinate
      seg_sampled[1, ] <- x[ldk_sorted[i], ]
    }

    result_segments[[i]] <- seg_sampled
    new_ldk[i] <- current_idx
    current_idx <- current_idx + seg_n_total
  }

  # Combine all segments
  res <- do.call(rbind, result_segments)

  list(coo = res, ldk = new_ldk)
}

.coo_sample_prop <- function(x, ldk = NULL, prop = 1, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))
  if (prop == 1) return(list(coo = x, ldk = ldk))
  n <- floor(prop * nrow(x))
  .coo_sample(x, ldk = ldk, n = n)
}


# coo_smooth ----

#' Smooth coordinates
#'
#' Smooth a shape using local averaging.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param n Integer. Number of smoothing iterations. Default is 0 (no smoothing).
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @details
#' - `coo_smooth()`: smooth all points
#' - `coo_smooth_fixed()`: smooth while keeping first and last points fixed
#'
#' @return
#' * If `x` is a single matrix: returns the smoothed matrix
#' * If `x` is a list: returns a list of smoothed matrices
#' * If `x` is a tibble: returns the tibble with specified coo columns smoothed
#'
#' @keywords internal
#' @export
coo_smooth <- make_coo_function(.coo_smooth)

#' @rdname coo_smooth
#' @export
coo_smooth_fixed <- make_coo_function(.coo_smooth_fixed)

.coo_smooth <- function(x, n = 0, ...) {
  if (!is.matrix(x)) return(x)
  if (n == 0) return(x)
  p <- nrow(x)

  for (i in seq_len(n)) {
    prev <- rbind(x[p, ], x[-p, ])
    next_pt <- rbind(x[-1, ], x[1, ])
    x <- x/2 + next_pt/4 + prev/4
  }

  x
}

.coo_smooth_fixed <- function(x, n = 0, ...) {
  if (!is.matrix(x)) return(x)
  if (n == 0) return(x)
  x0 <- x
  p <- nrow(x)

  for (i in seq_len(n)) {
    prev <- rbind(x[p, ], x[-p, ])
    next_pt <- rbind(x[-1, ], x[1, ])
    x <- x/2 + next_pt/4 + prev/4
    x[1, ] <- x0[1, ]
    x[p, ] <- x0[p, ]
  }

  x
}


# coo_baseline ----

#' Baseline correction (Bookstein registration)
#'
#' Align shape using baseline/Bookstein registration with specified landmarks.
#'
#' @param x A matrix (nx2) of coordinates.
#' @param id1 Integer. Index of first landmark point.
#' @param id2 Integer. Index of second landmark point.
#' @param t1 Numeric vector of length 2. Target position for first landmark.
#' @param t2 Numeric vector of length 2. Target position for second landmark.
#'
#' @details
#' `coo_bookstein()` is an alias for `coo_baseline()` with default target positions
#' at (-0.5, 0) and (0.5, 0) for standardized alignment.
#'
#' Note: These functions do not use the `make_coo_function` dispatcher as they
#' require specific landmark indices.
#'
#' @return A matrix with the baseline-corrected shape.
#'
#' @keywords internal
#' @export
coo_baseline <- function(x, id1 = 1, id2 = nrow(x),
                         t1 = c(-0.5, 0), t2 = c(0.5, 0)) {
  if (!is.matrix(x)) return(x)

  x <- .coo_translate(x, x_val = t1[1] - x[id1, 1], y_val = t1[2] - x[id1, 2])

  current_vec <- x[id2, ] - t1
  target_vec  <- t2 - t1

  scale <- sqrt(sum(target_vec^2)) / sqrt(sum(current_vec^2))
  theta <- atan2(target_vec[2], target_vec[1]) - atan2(current_vec[2], current_vec[1])

  x <- .coo_translate(x, x_val = -t1[1], y_val = -t1[2])
  x <- x * scale
  x <- .coo_rotate(x, theta)
  x <- .coo_translate(x, x_val = t1[1], y_val = t1[2])

  x
}

#' @rdname coo_baseline
#' @export
coo_bookstein <- function(x, id1 = 1, id2 = nrow(x)) {
  coo_baseline(x, id1 = id1, id2 = id2,
               t1 = c(-0.5, 0), t2 = c(0.5, 0))
}
