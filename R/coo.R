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
#' - Applies `xy` class to each single matrix
#' - Applies `coo` class to list and list-columns (ie when tibbles are passed)
#'
#' When `sync_ldk = TRUE`, the dispatcher automatically handles landmark columns:
#' - Looks for a column named `colname_ldk` (e.g., `coo_ldk` for `coo` column)
#' - Or uses `.ldk_col` argument if provided
#' - Passes landmarks to implementation function
#' - Updates landmarks based on coordinate changes
#'
#' When `sync_ldk = FALSE`, the implementation function should return just a matrix,
#' not a list with coo and ldk elements.
#'
#' Additional arguments are passed through via `...`
#'
#' @keywords internal
#' @export
make_coo_function <- function(impl_fn, fn_name = NULL, sync_ldk = FALSE) {
  # Infer function name if not provided (e.g., ".coo_center" from the impl function)
  if (is.null(fn_name)) {
    fn_name <- deparse(substitute(impl_fn))
  }

  f <- function(x, ..., .cols = NULL, .ldk_col = NULL) {

    # ===== TIBBLE CASE =====
    if (is.data.frame(x)) {
      # Capture .cols with tidyeval for bare column names
      .cols_quo <- rlang::enquo(.cols)
      if (rlang::quo_is_null(.cols_quo)) {
        # Auto-detect coo columns if .cols not specified
        cols_to_process <- get_coo_cols(x, NULL)
      } else {
        # Use specified columns
        cols_idx <- tidyselect::eval_select(.cols_quo, x)
        cols_to_process <- names(cols_idx)
      }

      # Process each coo column
      for (col in cols_to_process) {
        # Store original class to restore later (preserves custom classes like "out")
        original_class <- class(x[[col]])

        if (sync_ldk) {
          # Determine landmark column name with tidyeval support
          .ldk_col_quo <- rlang::enquo(.ldk_col)
          ldk_col_name <- if (!rlang::quo_is_null(.ldk_col_quo)) {
            rlang::as_name(.ldk_col_quo)
          } else {
            # Default: coo_ldk for column "coo"
            paste0(col, "_ldk")
          }

          if (ldk_col_name %in% names(x)) {
            # Process each shape with its landmarks
            results <- Map(function(coo, ldk) {
              impl_fn(coo, ldk = ldk, ...)
            }, x[[col]], x[[ldk_col_name]])

            # Extract transformed coordinates and updated landmarks
            x[[col]] <- lapply(results, `[[`, "coo")
            x[[ldk_col_name]] <- lapply(results, `[[`, "ldk")

            message(sprintf("\u2714 Updated landmarks in '%s'", ldk_col_name))
          } else {
            # No landmarks found, process without them
            results <- lapply(x[[col]], function(coo) {
              impl_fn(coo, ldk = NULL, ...)
            })
            x[[col]] <- lapply(results, `[[`, "coo")
          }
        } else {
          # No landmark support needed - simpler processing
          x[[col]] <- lapply(x[[col]], impl_fn, ...)
        }

        # Ensure each matrix in the list column has 'xy' class
        x[[col]] <- lapply(x[[col]], function(mat) {
          if (is.matrix(mat) && !"xy" %in% class(mat)) {
            class(mat) <- c("xy", class(mat))
          }
          mat
        })

        # Ensure 'coo' class on the list column itself (keep original classes too)
        if (!"coo" %in% original_class) {
          class(x[[col]]) <- c("coo", original_class)
        } else {
          class(x[[col]]) <- original_class
        }
      }
      return(x)
    }

    # ===== LIST CASE =====
    if (is.list(x)) {
      if (sync_ldk) {
        # Process each matrix with landmark support
        results <- lapply(x, function(coo) impl_fn(coo, ldk = NULL, ...))
        result_list <- lapply(results, `[[`, "coo")
      } else {
        # Process each matrix without landmark support
        result_list <- lapply(x, impl_fn, ...)
      }

      # Ensure each matrix in the list has 'xy' class
      result_list <- lapply(result_list, function(mat) {
        if (is.matrix(mat) && !"xy" %in% class(mat)) {
          class(mat) <- c("xy", class(mat))
        }
        mat
      })

      # Ensure 'coo' class on the list itself (keep original classes too)
      original_class <- class(x)
      if (!"coo" %in% original_class) {
        class(result_list) <- c("coo", original_class)
      } else {
        class(result_list) <- original_class
      }

      return(result_list)
    }

    # ===== SINGLE MATRIX CASE =====
    else {
      # Process the single matrix
      result <- if (sync_ldk) {
        impl_fn(x, ldk = NULL, ...)$coo
      } else {
        impl_fn(x, ...)
      }

      # Ensure 'xy' class is present on the matrix
      original_class <- class(result)
      if (!"xy" %in% original_class) {
        class(result) <- c("xy", original_class)
      }

      return(result)
    }
  }

  # Store metadata for display
  attr(f, "impl_fn") <- fn_name
  attr(f, "sync_ldk") <- sync_ldk

  # Custom class for nice printing of the function itself
  class(f) <- c("momocs2_function", "function")

  return(f)
}

# print.momocs2_function ----

#' @export
print.momocs2_function <- function(x, ...) {
  impl_fn <- attr(x, "impl_fn")
  sync_ldk <- attr(x, "sync_ldk")
  cat(sprintf("### Momocs2 function wrapping: %s ###\n", impl_fn))
  if (sync_ldk) {
    cat("### Landmark-aware: yes\n")
  }
  cat(sprintf("### View implementation: Momocs2:::%s ###\n\n", impl_fn))
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

# coo_check ----

#' Check and clean coordinates
#'
#' Remove problematic rows (NA values, consecutive duplicates, infinite values)
#' and warn about potential issues.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param remove_na Logical. Remove rows with NA values. Default TRUE.
#' @param remove_duplicates Logical. Remove consecutive duplicate points. Default TRUE.
#' @param remove_infinite Logical. Remove rows with Inf/-Inf values. Default TRUE.
#' @param min_points Integer. Warn if fewer than this many points remain. Default 3.
#' @param warn_collinear Logical. Warn if all points are collinear. Default TRUE.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#' @param .ldk_col Character. Name of landmark column. If `NULL`, uses `colname_ldk`.
#'
#' @return
#' * If `x` is a single matrix: returns the cleaned matrix
#' * If `x` is a list: returns a list of cleaned matrices
#' * If `x` is a tibble: returns the tibble with cleaned coo column(s)
#'
#' @details
#' Performs the following checks and cleaning operations:
#'
#' 1. **Remove NA rows**: Rows containing NA in either coordinate
#' 2. **Remove consecutive duplicates**: Identical adjacent points
#' 3. **Remove infinite values**: Rows containing Inf or -Inf
#' 4. **Warn if too few points**: Less than `min_points` remain
#' 5. **Warn if collinear**: All points lie on a line
#'
#' Messages indicate which shapes were affected and what was removed.
#' Landmarks are synced - if a point is removed, its landmark is also removed.
#'
#' @examples
#' # Clean a single shape
#' coo_check(shapes$cat)
#'
#' # Clean all shapes in list
#' coo_check(bot$coo)
#'
#' # Clean all shapes in tibble
#' coo_check(bot)
#'
#' # Disable specific checks
#' coo_check(bot, remove_duplicates = FALSE, warn_collinear = FALSE)
#'
#' @seealso [coo_sample()] for resampling
#'
#' @keywords internal
#' @export
coo_check <- make_coo_function(.coo_check, sync_ldk = TRUE)

.coo_check <- function(x, ldk = NULL,
                       remove_na = TRUE,
                       remove_duplicates = TRUE,
                       remove_infinite = TRUE,
                       min_points = 3,
                       warn_collinear = TRUE,
                       ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))

  original_n <- nrow(x)
  removed <- character(0)
  keep <- rep(TRUE, original_n)

  # Check for NA values
  if (remove_na) {
    na_rows <- apply(is.na(x), 1, any)
    if (any(na_rows)) {
      keep <- keep & !na_rows
      removed <- c(removed, sprintf("%d NA row%s", sum(na_rows),
                                    ifelse(sum(na_rows) > 1, "s", "")))
    }
  }

  # Check for infinite values
  if (remove_infinite) {
    inf_rows <- apply(is.infinite(x), 1, any)
    if (any(inf_rows)) {
      keep <- keep & !inf_rows
      removed <- c(removed, sprintf("%d infinite value%s", sum(inf_rows),
                                    ifelse(sum(inf_rows) > 1, "s", "")))
    }
  }

  # Check for consecutive duplicates
  if (remove_duplicates && nrow(x) > 1) {
    # Find consecutive duplicates
    dup_rows <- logical(original_n)
    for (i in 2:original_n) {
      if (isTRUE(all.equal(x[i, ], x[i-1, ]))) {
        dup_rows[i] <- TRUE
      }
    }
    if (any(dup_rows)) {
      keep <- keep & !dup_rows
      removed <- c(removed, sprintf("%d consecutive duplicate%s", sum(dup_rows),
                                    ifelse(sum(dup_rows) > 1, "s", "")))
    }
  }

  # Apply filtering
  x_clean <- x[keep, , drop = FALSE]

  # Update landmarks
  ldk_clean <- NULL
  if (!is.null(ldk) && length(ldk) > 0) {
    # Map old indices to new indices
    old_to_new <- cumsum(keep)
    old_to_new[!keep] <- NA

    # Keep only landmarks that still exist
    ldk_clean <- old_to_new[ldk]
    ldk_clean <- ldk_clean[!is.na(ldk_clean)]

    if (length(ldk_clean) < length(ldk)) {
      removed <- c(removed, sprintf("%d landmark%s",
                                    length(ldk) - length(ldk_clean),
                                    ifelse(length(ldk) - length(ldk_clean) > 1, "s", "")))
    }
  }

  # Report what was removed
  if (length(removed) > 0) {
    message(sprintf("\u2139 Removed: %s", paste(removed, collapse = ", ")))
  }

  # Warn if too few points remain
  if (nrow(x_clean) < min_points) {
    warning(sprintf("Only %d point%s remaining (minimum recommended: %d)",
                    nrow(x_clean),
                    ifelse(nrow(x_clean) > 1, "s", ""),
                    min_points))
  }

  # Warn if all points are collinear
  if (warn_collinear && nrow(x_clean) >= 3) {
    # Check collinearity using cross product
    v1 <- x_clean[2, ] - x_clean[1, ]
    collinear <- TRUE
    for (i in 3:nrow(x_clean)) {
      v2 <- x_clean[i, ] - x_clean[1, ]
      # Cross product in 2D
      cross <- v1[1] * v2[2] - v1[2] * v2[1]
      if (abs(cross) > 1e-10) {
        collinear <- FALSE
        break
      }
    }
    if (collinear) {
      warning("All points are collinear")
    }
  }

  list(coo = x_clean, ldk = ldk_clean)
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
#' # Extract specific points
#' coo_extract(shapes$cat, c(1, 10, 50))
#' coo_extract(bot$coo, c(1, 6))
#' coo_extract(bot, c(1, 5, 10))
#'
#' # Keep first n points
#' coo_head(shapes$cat, 10)
#' coo_head(bot$coo, 20)
#' coo_head(bot, 30)
#'
#' # Remove last n points
#' coo_tail(shapes$cat, 5)
#' coo_tail(bot, 10)
#'
#' @seealso [coo_sample()] for resampling to different point counts, [coo_trim_head()] for trimming from start
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

.coo_head <- function(x, ldk = NULL, n, ...) {
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

.coo_tail <- function(x, ldk = NULL, n, ...) {
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
#' # Single shape
#' coo_center(shapes$cat)
#'
#' # List of shapes
#' coo_center(bot$coo)
#'
#' # Tibble
#' coo_center(bot)
#'
#' @seealso [coo_translate()] for translation; [get_centroid()] for centroid coordinates
#'
#' @keywords internal
#' @export
coo_center <- make_coo_function(.coo_center)

.coo_center <- function(x, ...) {
  if (!is.matrix(x)) return(x)
  scale(x, center = TRUE, scale = FALSE)
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
#' # Single shape
#' coo_translate(shapes$cat, x_val = 10, y_val = 20)
#'
#' # List of shapes
#' coo_translate(bot$coo, x_val = 50)
#'
#' # Tibble
#' coo_translate(bot, x_val = 50, y_val = 50)
#'
#' @seealso [coo_center()] for centering
#'
#' @keywords internal
#' @export
coo_translate <- make_coo_function(.coo_translate)

.coo_translate <- function(x, x_val = 0, y_val = 0, ...) {
  if (!is.matrix(x)) return(x)
  cbind(x[, 1] + x_val, x[, 2] + y_val)
}


# coo_scale ----

#' Scale to unit centroid size
#'
#' Scale shape coordinates to unit centroid size (without centering).
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns the scaled matrix
#' * If `x` is a list: returns a list of scaled matrices
#' * If `x` is a tibble: returns the tibble with scaled coo column(s)
#'
#' @details
#' Scales coordinates to unit centroid size: divides all coordinates by centroid size.
#' Equivalent to `scale(x, center = FALSE, scale = TRUE)` in base R.
#'
#' Does NOT center the shape first. Use `coo_center()` before `coo_scale()` if needed.
#'
#' For rescaling by a custom factor, use `coo_rescale()`.
#'
#' @examples
#' # Single shape - scale to unit size
#' coo_scale(shapes$cat)
#'
#' # List of shapes
#' coo_scale(bot$coo)
#'
#' # Tibble - center then scale
#' bot %>% coo_center() %>% coo_scale()
#'
#' @seealso [coo_rescale()] for custom scaling; [coo_center()] for centering; [get_centroid_size()] for size measurement
#'
#' @keywords internal
#' @export
coo_scale <- make_coo_function(.coo_scale, sync_ldk = FALSE)

.coo_scale <- function(x, ...) {
  if (!is.matrix(x)) return(x)
  scale <- get_centroid_size_norm(x)
  cent <- get_centroid(x)
  x %>% .coo_center() %>% `/`(scale) %>% .coo_translate(cent[1], cent[2])
}


# coo_rescale ----

#' Rescale by a factor
#'
#' Multiply shape coordinates by a scaling factor.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param scale Numeric. Scaling factor to multiply coordinates by. If `x` is a tibble,
#'   can also be a column name (quoted or unquoted) containing scaling factors.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#' @param .ldk_col Character. Name of landmark column. If `NULL`, uses `colname_ldk`.
#'
#' @return
#' * If `x` is a single matrix: returns the rescaled matrix
#' * If `x` is a list: returns a list of rescaled matrices
#' * If `x` is a tibble: returns the tibble with rescaled coo column(s)
#'
#' @details
#' Multiplies all coordinates by a scaling factor. For tibbles, the scale factor
#' can come from a column (useful for rescaling based on stored measurements).
#'
#' This is pure multiplication - no centering or normalization.
#'
#' Landmarks are synced.
#'
#' @examples
#' # Double the size
#' coo_rescale(shapes$cat, scale = 2)
#'
#' # Halve the size
#' coo_rescale(bot$coo, scale = 0.5)
#'
#' # For tibbles - use stored scaling factors
#' bot$scale_factor <- rep(c(1, 2), length.out = nrow(bot))
#' bot %>% coo_rescale(scale = scale_factor)
#'
#' @seealso [coo_scale()] for normalizing to unit centroid size
#'
#' @keywords internal
#' @export
coo_rescale <- function(x, scale, ..., .cols = NULL, .ldk_col = NULL) {
  # Tibble case - handle scale from column
  if (is.data.frame(x)) {
    # Get coo column(s)
    .cols_quo <- rlang::enquo(.cols)
    if (rlang::quo_is_null(.cols_quo)) {
      cols_to_process <- get_coo_cols(x, NULL)
    } else {
      cols_idx <- tidyselect::eval_select(.cols_quo, x)
      cols_to_process <- names(cols_idx)
    }

    # Get landmark column name
    .ldk_col_quo <- rlang::enquo(.ldk_col)

    # Handle scale parameter
    scale_quo <- rlang::enquo(scale)

    # Try to evaluate as column name
    scale_values <- tryCatch({
      tidyselect::eval_select(scale_quo, x)
      x[[rlang::as_name(scale_quo)]]
    }, error = function(e) {
      # If not a column, evaluate as expression
      rlang::eval_tidy(scale_quo)
    })

    # If single value, recycle it
    if (length(scale_values) == 1) {
      scale_values <- rep(scale_values, nrow(x))
    }

    # Process each coo column
    for (col in cols_to_process) {
      # Get landmark column name
      ldk_col_name <- if (!rlang::quo_is_null(.ldk_col_quo)) {
        rlang::as_name(.ldk_col_quo)
      } else {
        paste0(col, "_ldk")
      }

      has_ldk <- ldk_col_name %in% names(x)

      # Rescale each shape
      for (i in seq_len(nrow(x))) {
        if (is.matrix(x[[col]][[i]])) {
          x[[col]][[i]] <- x[[col]][[i]] * scale_values[i]
        }
      }
    }

    return(x)
  }

  # Single matrix or list - use make_coo_function
  if (!is.numeric(scale) || length(scale) != 1) {
    stop("For matrices/lists, scale must be a single numeric value")
  }

  rescale_fn <- make_coo_function(.coo_rescale, sync_ldk = TRUE)
  rescale_fn(x, scale = scale, ..., .cols = .cols, .ldk_col = .ldk_col)
}

.coo_rescale <- function(x, ldk = NULL, scale, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))
  list(coo = x * scale, ldk = ldk)
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
#' # Rotate by 45 degrees
#' coo_rotate(shapes$cat, theta = pi / 4)
#'
#' # Rotate list of shapes
#' coo_rotate(bot$coo, theta = pi / 6)
#'
#' # Rotate tibble shapes
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


# coo_flip_xaxis ----

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
#' @examples
#' # Flip across x-axis
#' coo_flip_xaxis(shapes$cat)
#'
#' # Flip across y-axis
#' coo_flip_yaxis(bot$coo)
#'
#' # Flip across custom vertical line
#' coo_flip_x(shapes$cat, xintercept = 100)
#'
#' # Flip across custom horizontal line
#' coo_flip_y(bot, yintercept = 50)
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


# coo_close ----

#' Close or open outlines
#'
#' Close an outline by duplicating the first point at the end, or open by removing
#' duplicate endpoint.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo columns.
#'
#' @return
#' * If `x` is a single matrix: returns the matrix (closed or opened)
#' * If `x` is a list: returns a list of matrices
#' * If `x` is a tibble: returns the tibble with modified coo column(s)
#'
#' @details
#' **`coo_close()`**: If the last point is not identical to the first, adds the
#' first point at the end. If already closed, returns unchanged.
#'
#' **`coo_open()`**: If the last point is identical to the first, removes it.
#' If already open, returns unchanged.
#'
#' Landmarks are NOT adjusted - they keep their original indices.
#'
#' @examples
#' # Close a shape
#' coo_close(shapes$cat)
#'
#' # Open a closed shape
#' coo_open(shapes$cat)
#'
#' # Works on lists
#' coo_close(bot$coo)
#'
#' # Works on tibbles
#' coo_open(bot)
#'
#' @name coo_close
#' @seealso [coo_sample()] for resampling; [coo_close_spread()] for closing by spreading gap
#' @keywords internal
NULL

#' @rdname coo_close
#' @export
coo_close <- make_coo_function(.coo_close, sync_ldk = FALSE)

#' @rdname coo_close
#' @export
coo_open <- make_coo_function(.coo_open, sync_ldk = FALSE)

.coo_close <- function(x, ...) {
  if (!is.matrix(x)) return(x)

  # Check if already closed
  if (isTRUE(all.equal(x[1, ], x[nrow(x), ]))) {
    return(x)
  }

  # Add first point at end
  rbind(x, x[1, ])
}

.coo_open <- function(x, ...) {
  if (!is.matrix(x)) return(x)

  # Check if closed (last point = first point)
  if (isTRUE(all.equal(x[1, ], x[nrow(x), ]))) {
    # Remove last point
    return(x[-nrow(x), ])
  }

  # Already open
  x
}


# coo_close_spread ----

#' Close outline by spreading gap
#'
#' Distribute the distance between first and last points across all points to close the outline.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo columns.
#'
#' @return
#' * If `x` is a single matrix: returns the closed matrix
#' * If `x` is a list: returns a list of closed matrices
#' * If `x` is a tibble: returns the tibble with closed coo column(s)
#'
#' @details
#' For open outlines, distributes the gap between the first and last points
#' evenly across all points, so the outline becomes closed.
#'
#' @examples
#' # Close by spreading gap
#' coo_close_spread(shapes$cat)
#'
#' # Works on lists
#' coo_close_spread(bot$coo)
#'
#' # Works on tibbles
#' coo_close_spread(bot)
#'
#' @seealso [coo_close()] for simple closing; [coo_open()] for opening
#'
#' @keywords internal
#' @export
coo_close_spread <- make_coo_function(.coo_close_spread, sync_ldk = FALSE)

.coo_close_spread <- function(x, ...) {
  if (!is.matrix(x)) return(x)

  n <- nrow(x)

  # Calculate gap between first and last points
  gap <- x[1, ] - x[n, ]

  # Distribute gap evenly across all points
  adjustment <- outer(seq(0, 1, length.out = n), gap)

  x + adjustment
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
#' # Reverse single shape
#' coo_reverse(shapes$cat)
#'
#' # Reverse list of shapes
#' coo_reverse(bot$coo)
#'
#' # Reverse with landmarks
#' coo_reverse(hearts)
#'
#' @seealso [coo_direction_positive()] for ensuring counter-clockwise direction; [coo_slide_id()] for reordering
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


# coo_direction_positive ----

#' Ensure positive (counter-clockwise) direction
#'
#' Reverse the outline if it's traced clockwise to ensure counter-clockwise direction.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo columns.
#' @param .ldk_col Character. Name of landmark column. If `NULL`, uses `colname_ldk`.
#'
#' @return
#' * If `x` is a single matrix: returns the matrix (reversed if needed)
#' * If `x` is a list: returns a list of matrices
#' * If `x` is a tibble: returns the tibble with direction-corrected coo column(s)
#'
#' @details
#' **`coo_direction_positive()`**: Ensures outline is traced counter-clockwise
#' (trigonometric/positive direction). If clockwise, reverses point order and syncs landmarks.
#'
#' **`coo_direction_negative()`**: Ensures outline is traced clockwise
#' (negative direction). If counter-clockwise, reverses point order and syncs landmarks.
#'
#' @examples
#' # Ensure counter-clockwise
#' coo_direction_positive(shapes$cat)
#'
#' # Ensure clockwise
#' coo_direction_negative(bot$coo)
#'
#' # Works with landmarks
#' coo_direction_positive(hearts)
#'
#' @name coo_direction
#' @seealso [get_direction_sign()] for checking direction; [coo_reverse()] for reversing
#' @keywords internal
NULL

#' @rdname coo_direction
#' @export
coo_direction_positive <- make_coo_function(.coo_direction_positive, sync_ldk = TRUE)

#' @rdname coo_direction
#' @export
coo_direction_negative <- make_coo_function(.coo_direction_negative, sync_ldk = TRUE)

.coo_direction_positive <- function(x, ldk = NULL, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))

  # Check direction
  if (.get_direction_sign(x)) {
    # Already positive (counter-clockwise)
    list(coo = x, ldk = ldk)
  } else {
    # Clockwise - reverse it
    .coo_reverse(x, ldk = ldk)
  }
}

.coo_direction_negative <- function(x, ldk = NULL, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))

  # Check direction
  if (.get_direction_sign(x)) {
    # Counter-clockwise - reverse it to make clockwise
    .coo_reverse(x, ldk = ldk)
  } else {
    # Already negative (clockwise)
    list(coo = x, ldk = ldk)
  }
}


# coo_slide_id ----

#' Slide coordinates along outline
#'
#' Rotate the order of points along the outline without changing the shape.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param id Integer. Index of point to become the first point. Default is 1.
#' @param ldk_id Integer. Index of landmark (in ldk column) to become first point.
#' @param target Numeric vector of length 2 (x, y). Find closest point to target.
#' @param theta Numeric. Angle in radians. Find point closest to this angle.
#' @param direction Character. Direction ("right", "up", "left", "down").
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#' @param .ldk_col Character. Name of landmark column. If `NULL`, uses `colname_ldk`.
#'
#' @details
#' - `coo_slide_id()`: slide to specific point index
#' - `coo_slide_ldk()`: slide to make ith landmark become first point
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
#' @examples
#' # Slide to point 50
#' coo_slide_id(shapes$cat, id = 50)
#'
#' # Slide to first landmark
#' coo_slide_ldk(hearts, ldk_id = 1)
#'
#' # Slide to point closest to coordinates
#' coo_slide_closest(shapes$cat, target = c(200, 100))
#'
#' # Slide to rightmost point
#' coo_slide_direction(bot$coo, direction = "right")
#'
#' # Slide to largest gap
#' coo_slide_gap(shapes$cat)
#'
#' @name coo_slide
#' @seealso [coo_reverse()] for reversing point order; [coo_sample()] for resampling
#' @keywords internal
NULL

#' @rdname coo_slide
#' @export
coo_slide_id <- make_coo_function(.coo_slide_id, sync_ldk = TRUE)

#' @rdname coo_slide
#' @export
coo_slide_ldk <- make_coo_function(.coo_slide_ldk, sync_ldk = TRUE)

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

.coo_slide_id <- function(x, ldk = NULL, id = 1, ...) {
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

.coo_slide_ldk <- function(x, ldk = NULL, ldk_id = 1, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))

  if (is.null(ldk) || length(ldk) == 0) {
    stop("No landmarks available. Use coo_slide_id() for shapes without landmarks.")
  }

  if (ldk_id < 1 || ldk_id > length(ldk)) {
    stop("ldk_id must be between 1 and ", length(ldk))
  }

  # Get the point index of the target landmark
  point_id <- ldk[ldk_id]

  # Slide to that point
  .coo_slide_id(x, ldk = ldk, id = point_id)
}

.coo_slide_closest <- function(x, ldk = NULL, target, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))
  closest_id <- .get_closest(x, target)
  .coo_slide_id(x, ldk = ldk, id = closest_id)
}

.coo_slide_angle <- function(x, ldk = NULL, theta = 0, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))
  closest_id <- .get_closest_angle(x, theta = theta)
  .coo_slide_id(x, ldk = ldk, id = closest_id)
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
  .coo_slide_id(x, ldk = ldk, id = gap_id + 1)
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
#' @examples
#' # Resample to 64 points
#' coo_sample(shapes$cat, n = 64)
#'
#' # Resample list
#' coo_sample(bot$coo, n = 100)
#'
#' # Resample with landmarks preserved
#' coo_sample(hearts, n = 200)
#'
#' # Resample to 50% of points
#' coo_sample_prop(shapes$cat, prop = 0.5)
#'
#' @seealso [get_coords_nb()] for point count; [coo_smooth()] for smoothing; [coo_sample_regular_radius()] for radial sampling
#'
#' @name coo_sample
#' @keywords internal
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


# coo_sample_regular_radius ----

#' Sample at regular angles from centroid
#'
#' Sample points at regular angular intervals from the centroid (radial sampling).
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param n Integer. Number of points to sample.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo columns.
#' @param .ldk_col Character. Name of landmark column. If `NULL`, uses `colname_ldk`.
#'
#' @return
#' * If `x` is a single matrix: returns the sampled matrix
#' * If `x` is a list: returns a list of sampled matrices
#' * If `x` is a tibble: returns the tibble with sampled coo column(s)
#'
#' @details
#' Samples points at regular angular intervals from the centroid, like spokes
#' on a wheel. For each target angle, finds the outline point closest to that
#' direction.
#'
#' Landmarks are NOT preserved - this resampling method fundamentally changes
#' the point structure.
#'
#' @examples
#' # Sample at 64 regular angles
#' coo_sample_regular_radius(shapes$cat, n = 64)
#'
#' # Works on lists
#' coo_sample_regular_radius(bot$coo, n = 32)
#'
#' # Works on tibbles
#' coo_sample_regular_radius(bot, n = 128)
#'
#' @seealso [coo_sample()] for arc-length sampling
#'
#' @keywords internal
#' @export
coo_sample_regular_radius <- make_coo_function(.coo_sample_regular_radius, sync_ldk = TRUE)

.coo_sample_regular_radius <- function(x, ldk = NULL, n = 64, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))

  # Center shape
  cent <- colMeans(x)
  x_centered <- sweep(x, 2, cent)

  # Calculate angles for all points
  angles <- atan2(x_centered[, 2], x_centered[, 1])

  # Target angles (regular intervals)
  target_angles <- seq(0, 2*pi, length.out = n + 1)[-(n + 1)]

  # For each target angle, find closest point
  sampled_ids <- vapply(target_angles, function(theta) {
    angle_diff <- abs(angles - theta)
    # Handle circular distance
    angle_diff <- pmin(angle_diff, 2*pi - angle_diff)
    which.min(angle_diff)
  }, integer(1))

  list(coo = x[sampled_ids, ], ldk = NULL)
}


# coo_trim ----

#' Trim points from outline ends
#'
#' Remove points from the start (head), end (tail), or both ends of an outline.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param n Integer or numeric. Number of points to remove, or if between 0 and 1,
#'   fraction of perimeter to remove.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo columns.
#'
#' @return
#' * If `x` is a single matrix: returns the trimmed matrix
#' * If `x` is a list: returns a list of trimmed matrices
#' * If `x` is a tibble: returns the tibble with trimmed coo column(s)
#'
#' @details
#' **`coo_trim_head(n)`**: Removes `n` points from the start (head).
#'
#' **`coo_trim_tail(n)`**: Removes `n` points from the end (tail).
#'
#' **`coo_trim_both(n)`**: Removes `n` points from each end (2n total).
#'
#' If `n` is between 0 and 1, interprets as fraction of perimeter to remove.
#'
#' Landmarks are NOT adjusted. Use with caution if shape has landmarks.
#'
#' @examples
#' # Remove 10 points from start
#' coo_trim_head(shapes$cat, n = 10)
#'
#' # Remove 5 points from end
#' coo_trim_tail(bot$coo, n = 5)
#'
#' # Remove 5% of perimeter from each end
#' coo_trim_both(shapes$cat, n = 0.05)
#'
#' @name coo_trim
#' @seealso [coo_head()] for keeping first n points; [coo_tail()] for removing last n points
#' @keywords internal
NULL

#' @rdname coo_trim
#' @export
coo_trim_head <- make_coo_function(.coo_trim_head, sync_ldk = FALSE)

#' @rdname coo_trim
#' @export
coo_trim_tail <- make_coo_function(.coo_trim_tail, sync_ldk = FALSE)

#' @rdname coo_trim
#' @export
coo_trim_both <- make_coo_function(.coo_trim_both, sync_ldk = FALSE)

.coo_trim_head <- function(x, n = 1, ...) {
  if (!is.matrix(x)) return(x)

  # If n is fraction, convert to number of points
  if (n > 0 && n < 1) {
    perim <- .get_perim(x)
    perim_cum <- c(0, cumsum(sqrt(rowSums(diff(rbind(x, x[1, ]))^2))))
    n_points <- which(perim_cum >= n * perim)[1] - 1
    n <- max(1, n_points)
  }

  n <- as.integer(n)
  if (n >= nrow(x)) {
    stop("Cannot trim more points than available")
  }

  x[-(1:n), ]
}

.coo_trim_tail <- function(x, n = 1, ...) {
  if (!is.matrix(x)) return(x)

  # If n is fraction, convert to number of points
  if (n > 0 && n < 1) {
    perim <- .get_perim(x)
    perim_cum <- c(0, cumsum(sqrt(rowSums(diff(rbind(x, x[1, ]))^2))))
    n_points <- which(perim_cum >= n * perim)[1] - 1
    n <- max(1, n_points)
  }

  n <- as.integer(n)
  if (n >= nrow(x)) {
    stop("Cannot trim more points than available")
  }

  nr <- nrow(x)
  x[-((nr-n+1):nr), ]
}

.coo_trim_both <- function(x, n = 1, ...) {
  if (!is.matrix(x)) return(x)

  # If n is fraction, convert to number of points
  if (n > 0 && n < 1) {
    perim <- .get_perim(x)
    perim_cum <- c(0, cumsum(sqrt(rowSums(diff(rbind(x, x[1, ]))^2))))
    n_points <- which(perim_cum >= n * perim)[1] - 1
    n <- max(1, n_points)
  }

  n <- as.integer(n)
  if (2 * n >= nrow(x)) {
    stop("Cannot trim more points than available")
  }

  nr <- nrow(x)
  x[-c(1:n, (nr-n+1):nr), ]
}


# coo_up/down/left/right ----

#' Filter points by direction from centroid
#'
#' Keep only points in a specific direction relative to the centroid.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo columns.
#' @param .ldk_col Character. Name of landmark column. If `NULL`, uses `colname_ldk`.
#'
#' @return
#' * If `x` is a single matrix: returns the filtered matrix (open curve)
#' * If `x` is a list: returns a list of filtered matrices
#' * If `x` is a tibble: returns the tibble with filtered coo column(s)
#'
#' @details
#' Filters points based on their position relative to the centroid:
#' - **`coo_up()`**: Keep points with y > centroid_y
#' - **`coo_down()`**: Keep points with y < centroid_y
#' - **`coo_right()`**: Keep points with x > centroid_x
#' - **`coo_left()`**: Keep points with x < centroid_x
#'
#' Returns an open curve. Landmarks are synced.
#'
#' @examples
#' # Keep upper half
#' coo_up(shapes$cat)
#'
#' # Keep right half
#' coo_right(bot$coo)
#'
#' # Keep lower half
#' coo_down(shapes$cat)
#'
#' # Keep left half with landmarks
#' coo_left(hearts)
#'
#' @name coo_direction_filter
#' @seealso [coo_extract()] for extracting specific points
#' @keywords internal
NULL

#' @rdname coo_direction_filter
#' @export
coo_up <- make_coo_function(.coo_up, sync_ldk = TRUE)

#' @rdname coo_direction_filter
#' @export
coo_down <- make_coo_function(.coo_down, sync_ldk = TRUE)

#' @rdname coo_direction_filter
#' @export
coo_right <- make_coo_function(.coo_right, sync_ldk = TRUE)

#' @rdname coo_direction_filter
#' @export
coo_left <- make_coo_function(.coo_left, sync_ldk = TRUE)

.coo_up <- function(x, ldk = NULL, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))

  cent <- colMeans(x)
  keep <- x[, 2] > cent[2]

  list(coo = x[keep, , drop = FALSE], ldk = ldk)
}

.coo_down <- function(x, ldk = NULL, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))

  cent <- colMeans(x)
  keep <- x[, 2] < cent[2]

  list(coo = x[keep, , drop = FALSE], ldk = ldk)
}

.coo_right <- function(x, ldk = NULL, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))

  cent <- colMeans(x)
  keep <- x[, 1] > cent[1]

  list(coo = x[keep, , drop = FALSE], ldk = ldk)
}

.coo_left <- function(x, ldk = NULL, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))

  cent <- colMeans(x)
  keep <- x[, 1] < cent[1]

  list(coo = x[keep, , drop = FALSE], ldk = ldk)
}


# coo_untilt_x ----

#' Untilt to axis
#'
#' Rotate shape so the first point is exactly on an axis.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo columns.
#'
#' @return
#' * If `x` is a single matrix: returns the rotated matrix
#' * If `x` is a list: returns a list of rotated matrices
#' * If `x` is a tibble: returns the tibble with rotated coo column(s)
#'
#' @details
#' **`coo_untilt_x()`**: Rotates around centroid so first point is on x-axis (y=0).
#'
#' **`coo_untilt_y()`**: Rotates around centroid so first point is on y-axis (x=0),
#' choosing the nearest y-axis (positive or negative).
#'
#' Works best when shapes are centered.
#'
#' @examples
#' # Untilt to x-axis
#' coo_untilt_x(shapes$cat)
#'
#' # Untilt to y-axis
#' coo_untilt_y(bot$coo)
#'
#' # Works better after centering
#' shapes$cat %>% coo_center() %>% coo_untilt_x()
#'
#' @name coo_untilt
#' @seealso [coo_rotate()] for arbitrary rotation; [coo_align()] for principal axis alignment
#' @keywords internal
NULL

#' @rdname coo_untilt
#' @export
coo_untilt_x <- make_coo_function(.coo_untilt_x, sync_ldk = FALSE)

#' @rdname coo_untilt
#' @export
coo_untilt_y <- make_coo_function(.coo_untilt_y, sync_ldk = FALSE)

.coo_untilt_x <- function(x, ...) {
  if (!is.matrix(x)) return(x)

  # Center
  cent <- colMeans(x)
  x_centered <- sweep(x, 2, cent)

  # Angle of first point
  angle <- atan2(x_centered[1, 2], x_centered[1, 1])

  # Rotate by -angle to put on x-axis
  .coo_rotate(x, theta = -angle)
}

.coo_untilt_y <- function(x, ...) {
  if (!is.matrix(x)) return(x)

  # Center
  cent <- colMeans(x)
  x_centered <- sweep(x, 2, cent)

  # Angle of first point
  angle <- atan2(x_centered[1, 2], x_centered[1, 1])

  # Rotate to nearest y-axis (pi/2 or -pi/2)
  # Determine which is closer
  if (abs(angle - pi/2) < abs(angle + pi/2)) {
    target_angle <- pi/2
  } else {
    target_angle <- -pi/2
  }

  .coo_rotate(x, theta = target_angle - angle)
}


# coo_align_calliper ----

#' Align to special features
#'
#' Align shape to x-axis based on specific geometric features.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo columns.
#'
#' @return
#' * If `x` is a single matrix: returns the aligned matrix
#' * If `x` is a list: returns a list of aligned matrices
#' * If `x` is a tibble: returns the tibble with aligned coo column(s)
#'
#' @details
#' **`coo_align_calliper()`**: Aligns the calliper (maximum distance between points)
#' horizontally along the x-axis.
#'
#' **`coo_align_minradius()`**: Aligns the point with minimum distance to centroid
#' (min radius) to the x-axis.
#'
#' @examples
#' # Align calliper to x-axis
#' coo_align_calliper(shapes$cat)
#'
#' # Align min radius to x-axis
#' coo_align_minradius(bot$coo)
#'
#' @name coo_align_features
#' @seealso [coo_align()] for principal axis alignment; [get_calliper()] for calliper measurement
#' @keywords internal
NULL

#' @rdname coo_align_features
#' @export
coo_align_calliper <- make_coo_function(.coo_align_calliper, sync_ldk = FALSE)

#' @rdname coo_align_features
#' @export
coo_align_minradius <- make_coo_function(.coo_align_minradius, sync_ldk = FALSE)

.coo_align_calliper <- function(x, ...) {
  if (!is.matrix(x)) return(x)

  # Get calliper point indices
  ids <- .get_calliper_ids(x)

  # Vector between these points
  v <- x[ids[2], ] - x[ids[1], ]

  # Angle of this vector
  angle <- atan2(v[2], v[1])

  # Rotate to x-axis
  .coo_rotate(x, theta = -angle)
}

.coo_align_minradius <- function(x, ...) {
  if (!is.matrix(x)) return(x)

  # Center shape
  cent <- colMeans(x)
  x_centered <- sweep(x, 2, cent)

  # Find point with minimum radius
  radii <- sqrt(rowSums(x_centered^2))
  min_id <- which.min(radii)

  # Angle of this point
  angle <- atan2(x_centered[min_id, 2], x_centered[min_id, 1])

  # Rotate to x-axis
  .coo_rotate(x, theta = -angle)
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
#' @examples
#' # Smooth with 3 iterations
#' coo_smooth(shapes$cat, n = 3)
#'
#' # Smooth list
#' coo_smooth(bot$coo, n = 5)
#'
#' # Smooth with fixed endpoints
#' coo_smooth_fixed(shapes$cat, n = 3)
#'
#' @name coo_smooth
#' @seealso [coo_sample()] for resampling
#' @keywords internal
NULL

#' @rdname coo_smooth
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
#' @examples
#' # Center and translate to origin
#' coo_center_to(shapes$cat, x_val = 0, y_val = 0)
#'
#' # Center and translate to (100, 100)
#' coo_center_to(bot$coo, x_val = 100, y_val = 100)
#'
#' @seealso [coo_center()] for centering only; [coo_translate()] for translation only
#' @keywords internal
#' @export
coo_center_to <- make_coo_function(.coo_center_to)

.coo_center_to <- function(x, x_val = 0, y_val = 0, ...) {
  if (!is.matrix(x)) return(x)
  x %>% .coo_center() %>% .coo_translate(x_val = x_val, y_val = y_val)
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
#' @examples
#' # Translate to x-axis
#' coo_translate_to_xaxis(shapes$cat)
#'
#' # Works on lists
#' coo_translate_to_xaxis(bot$coo)
#'
#' @seealso [coo_translate_to_yaxis()] for y-axis; [coo_center()] for centering
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
#' @examples
#' # Translate to y-axis
#' coo_translate_to_yaxis(shapes$cat)
#'
#' # Works on tibbles
#' coo_translate_to_yaxis(bot)
#'
#' @seealso [coo_translate_to_xaxis()] for x-axis; [coo_center()] for centering
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
#' @examples
#' # Jitter by default amount
#' coo_translate_jitter(shapes$cat)
#'
#' # Custom jitter amount
#' coo_translate_jitter(bot$coo, amount = 0.2)
#'
#' @seealso [coo_translate()] for deterministic translation
#'
#' @keywords internal
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
#' coo_affine(bot$coo, scale_mat)
#'
#' @seealso [coo_rotate()] for rotation; [coo_shear_x()] for shearing; [coo_flip_xaxis()] for flipping
#'
#' @keywords internal
#' @export
coo_affine <- make_coo_function(.coo_affine)

.coo_affine <- function(x, mat, ...) {
  if (!is.matrix(x)) return(x)
  x %*% mat
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
#' @examples
#' # Rotate around centroid
#' coo_rotate_around(shapes$cat, theta = pi/4)
#'
#' # Rotate around custom point
#' coo_rotate_around(bot$coo, theta = pi/6, center = c(100, 100))
#'
#' @seealso [coo_rotate()] for rotation around origin
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
#' @examples
#' # Align to principal axes
#' coo_align(shapes$cat)
#'
#' # Works on lists
#' coo_align(bot$coo)
#'
#' @seealso [coo_align_minor()] for swapped axes; [coo_rotate()] for arbitrary rotation
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
#' @examples
#' # Align with swapped axes
#' coo_align_minor(shapes$cat)
#'
#' # Works on tibbles
#' coo_align_minor(bot)
#'
#' @seealso [coo_align()] for standard alignment
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
#' @examples
#' # Shear in x direction
#' coo_shear_x(shapes$cat, k = 0.5)
#'
#' # Shear in y direction
#' coo_shear_y(bot$coo, k = 0.3)
#'
#' @seealso [coo_rotate()] for rotation; [coo_flip_xaxis()] for flipping; [coo_affine()] for custom transformations
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
#' @examples
#' # Bookstein registration using first and last points
#' coo_bookstein(shapes$cat, id1 = 1, id2 = 120)
#'
#' # Custom baseline with target positions
#' coo_baseline(shapes$cat, id1 = 1, id2 = 60, t1 = c(0, 0), t2 = c(1, 0))
#'
#' @name coo_baseline
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


# coo_template ----

#' Template shapes to standard size
#'
#' Scale and center shapes to fit in a standard template box.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param size Numeric. Target size for the largest dimension. Default is 1.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return
#' * If `x` is a single matrix: returns the templated matrix
#' * If `x` is a list: returns a list of templated matrices
#' * If `x` is a tibble: returns the tibble with templated coo column(s)
#'
#' @details
#' **`coo_template()`**: Scales each shape independently so its largest dimension
#' equals `size`, then centers it at the origin (0, 0).
#'
#' **`coo_template_relatively()`**: Scales shapes while preserving relative sizes.
#' The largest shape gets `size`, others are scaled proportionally smaller.
#' All shapes are centered at (0, 0).
#'
#' For a single matrix, both functions produce identical results.
#'
#' `coo_template_relatively()` is perfect for `mosaic()` to create family pictures
#' where size relationships are preserved!
#'
#' @examples
#' # Single shape - both functions are identical
#' coo_template(shapes$cat, size = 1)
#' coo_template_relatively(shapes$cat, size = 1)
#'
#' # Multiple shapes - independent scaling
#' coo_template(bot$coo, size = 1)
#'
#' # Multiple shapes - relative scaling
#' coo_template_relatively(bot$coo, size = 1)
#'
#' # Perfect for mosaic - shows true size relationships
#' bot %>%
#'   coo_template_relatively(size = 1) %>%
#'   mosaic()
#'
#' @name coo_template
#' @seealso [mosaic()] for visualization; [coo_center()] for centering; [coo_scale()] for normalization
#' @keywords internal
NULL

#' @rdname coo_template
#' @export
coo_template <- make_coo_function(.coo_template, sync_ldk = FALSE)

.coo_template <- function(x, size = 1, ...) {

  # Calculate ranges
  x_range <- diff(range(x[, 1]))
  y_range <- diff(range(x[, 2]))

  # Scale factor based on largest dimension
  scale_factor <- size / max(x_range, y_range)

  # Scale coordinates
  coo_scaled <- x * scale_factor

  # Center at origin
  # Expected center position (half of scaled ranges)
  expected_center <- c(
    diff(range(coo_scaled[, 1])) / 2,
    diff(range(coo_scaled[, 2])) / 2
  )

  # Observed center (current max values)
  observed_center <- c(
    max(coo_scaled[, 1]),
    max(coo_scaled[, 2])
  )

  # Shift to center at origin
  shift <- expected_center - observed_center
  coo_centered <- sweep(coo_scaled, 2, shift, "+")

  # return this beauty
  coo_centered
}


#' @rdname coo_template
#' @export
coo_template_relatively <- function(x, size = 1, ..., .cols = NULL, .ldk_col = NULL) {
  # Single matrix - just use regular template
  if (is.matrix(x)) {
    return(coo_template(x, size = size, ..., .cols = .cols, .ldk_col = .ldk_col))
  }

  # List case - need special handling for relative sizes
  if (is.list(x) && !is.data.frame(x)) {
    # Calculate "length" (max dimension) for each shape
    lengths <- vapply(x, function(coo) {
      if (!is.matrix(coo)) return(NA_real_)
      x_range <- diff(range(coo[, 1]))
      y_range <- diff(range(coo[, 2]))
      max(x_range, y_range)
    }, numeric(1))

    # Find largest shape
    max_length <- max(lengths, na.rm = TRUE)

    # Calculate relative sizes
    rel_sizes <- (lengths / max_length) * size

    # Template each shape with its relative size
    result <- lapply(seq_along(x), function(i) {
      if (!is.matrix(x[[i]])) return(x[[i]])
      coo_template(x[[i]], size = rel_sizes[i], ...)
    })

    names(result) <- names(x)
    return(result)
  }

  # Tibble case - use standard dispatcher
  if (is.data.frame(x)) {
    # Get coo column
    .cols_quo <- rlang::enquo(.cols)
    if (rlang::quo_is_null(.cols_quo)) {
      cols_to_process <- get_coo_cols(x, NULL)
    } else {
      cols_idx <- tidyselect::eval_select(.cols_quo, x)
      cols_to_process <- names(cols_idx)
    }

    col <- cols_to_process[1]

    # Extract list, template relatively, put back
    coo_list <- x[[col]]
    templated <- coo_template_relatively(coo_list, size = size, ...)
    x[[col]] <- templated

    return(x)
  }

  # Fallback
  x
}


# get_transformed ----

#' Apply transformation to coordinates
#'
#' Apply a transformation (scale, rotation, translation) to shape coordinates.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param transform A list with elements `scale`, `rotation`, and `translation`,
#'   typically from `get_transform()`.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo columns.
#' @param .ldk_col Character. Name of landmark column. If `NULL`, uses `colname_ldk`.
#'
#' @return
#' * If `x` is a single matrix: returns the transformed matrix
#' * If `x` is a list: returns a list of transformed matrices
#' * If `x` is a tibble: returns the tibble with transformed coo column(s)
#'
#' @details
#' Applies transformation in order:
#' 1. Scale (multiply coordinates by scale factor)
#' 2. Rotate (by rotation angle in radians)
#' 3. Translate (shift by translation vector)
#'
#' Landmarks are transformed along with coordinates.
#'
#' @examples
#' source <- matrix(c(0,0, 1,0, 1,1, 0,1), ncol=2, byrow=TRUE)
#' target <- source * 2  # Scaled by 2
#'
#' # Extract transformation
#' transform <- get_transform(source, target)
#'
#' # Apply to another shape
#' new_shape <- matrix(c(0,0, 2,0, 2,2), ncol=2, byrow=TRUE)
#' transformed <- get_transformed(new_shape, transform)
#'
#' @seealso [get_transform()] for extracting transformations
#'
#' @keywords internal
#' @export
get_transformed <- make_coo_function(.get_transformed, sync_ldk = TRUE)

.get_transformed <- function(x, ldk = NULL, transform, ...) {
  if (!is.matrix(x)) return(list(coo = x, ldk = ldk))

  if (!is.list(transform) ||
      !all(c("scale", "rotation", "translation") %in% names(transform))) {
    stop("transform must be a list with scale, rotation, and translation")
  }

  # Apply scale
  x_scaled <- x * transform$scale

  # Apply rotation
  cos_r <- cos(transform$rotation)
  sin_r <- sin(transform$rotation)
  x_rotated <- cbind(
    x_scaled[, 1] * cos_r - x_scaled[, 2] * sin_r,
    x_scaled[, 1] * sin_r + x_scaled[, 2] * cos_r
  )

  # Apply translation
  x_transformed <- sweep(x_rotated, 2, transform$translation, "+")

  list(coo = x_transformed, ldk = ldk)
}
