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


# get_coe ------

#' Identify coe columns in a tibble

#' Get coefficient column name(s)
#'
#' Detect and return the name of coefficient column(s) in a tibble. These functions
#' look for columns with class `"coe"` in their class hierarchy.
#'
#' @param df A data frame or tibble
#' @param .cols Optional. Explicitly specify which column(s) to use. Can be:
#'   * Character vector of column names
#'   * Integer vector of column positions
#'   * Logical vector
#'   If `NULL` (default), automatically detects columns with class `"coe"`.
#'
#' @return
#' * `get_coe_cols()`: Character string with single column name
#' * `get_all_coe_cols()`: Character vector with all coe column names
#'
#' @details
#' ## Detection strategy
#'
#' Both functions look for columns that have `"coe"` anywhere in their class
#' hierarchy. This includes:
#' * Columns with class `c("coe", "list")`
#' * Columns with class `c("eft", "coe", "list")`
#' * Columns with class `c("out", "coe", "list")`
#' * Any other combination where `"coe"` is present
#'
#' All morphometric methods automatically add the `"coe"` class to their output columns.
#'
#' ## Differences between functions
#'
#' * **`get_coe_cols()`**: Returns a single column name. Errors if:
#'   - No `"coe"` columns found
#'   - Multiple `"coe"` columns found (user must specify `.cols`)
#'
#' * **`get_all_coe_cols()`**: Returns all coe column names as a vector. Errors only if:
#'   - No `"coe"` columns found
#'
#' ## Manual specification
#'
#' If automatic detection fails or you want to use a specific column,
#' use the `.cols` argument in `get_coe_cols()`:
#'
#' ```r
#' get_coe_cols(df, .cols = "my_coefficients")
#' get_coe_cols(df, .cols = 2)  # Second column
#' ```
#'
#' @examples
#' # Create sample data with coefficient columns
#' df <- tibble::tibble(
#'   id = 1:3,
#'   coe = list(1:5, 2:6, 3:7)
#' )
#' class(df$coe) <- c("eft", "coe", "list")
#'
#' # Get single coe column
#' get_coe_cols(df)
#' # [1] "coe"
#'
#' # Multiple coe columns
#' df$coe2 <- df$coe
#' class(df$coe2) <- c("coe", "list")
#'
#' # get_coe_cols() errors with multiple
#' try(get_coe_cols(df))
#' # Error: Multiple coe columns found: coe, coe2
#'
#' # But can specify explicitly
#' get_coe_cols(df, .cols = "coe")
#' # [1] "coe"
#'
#' # get_all_coe_cols() returns all
#' get_all_coe_cols(df)
#' # [1] "coe"  "coe2"
#'
#' @seealso [fold()], [unfold()], [eft()]
#'
#' @export
get_coe_cols <- function(df, .cols = NULL) {
  if (!is.null(.cols)) {
    # User explicitly specified which columns
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

  # Look for columns with class "coe"
  coe_cols <- names(df)[vapply(df, function(col) "coe" %in% class(col), logical(1))]

  if (length(coe_cols) == 0) {
    stop("No columns with class 'coe' found. Use .cols argument to specify explicitly.")
  }

  if (length(coe_cols) > 1) {
    stop(sprintf("Multiple coe columns found: %s. Use .cols to specify which one.",
                 paste(coe_cols, collapse = ", ")))
  }

  coe_cols
}


#' @rdname get_coe_cols
#' @export
get_all_coe_cols <- function(df) {
  # Look for all columns with class "coe"
  coe_cols <- names(df)[vapply(df, function(col) "coe" %in% class(col), logical(1))]

  if (length(coe_cols) == 0) {
    stop("No columns with class 'coe' found.")
  }

  coe_cols
}
