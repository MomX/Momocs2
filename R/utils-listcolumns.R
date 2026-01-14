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


# get_coe_cols ----

#' Identify coe columns in a tibble
#'
#' Detect which columns in a tibble/data.frame contain coefficient objects
#' or list columns of numeric vectors/matrices.
#'
#' @param df A tibble or data.frame.
#' @param .cols Character vector or NULL. If specified, use these column names.
#'   If NULL, auto-detect columns containing coe objects.
#'
#' @return Character vector of column names to process.
#'
#' @details
#' Detection priority:
#' 1. Columns with class "coe" in their class hierarchy (e.g., c("eft", "coe", "list"))
#' 2. List columns where all elements are numeric vectors or matrices
#'
#' When multiple qualifying columns exist, an error is raised and user must
#' specify `.cols` explicitly.
#'
#' @examples
#' #bot %>% eft() %>% get_coe_cols
#' #get_coe_cols(boteft, "coe")
#'
#' @seealso [get_coo_cols()] for coordinate columns
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

  # Helper to check if column contains numeric vectors/matrices
  is_numeric_list <- function(col) {
    if (!is.list(col) || length(col) == 0) return(FALSE)
    # Check if all elements are numeric vectors or matrices
    all(sapply(col, function(x) is.numeric(x) || is.matrix(x)))
  }

  # First, look for columns with class "coe" (anywhere in class hierarchy)
  coe_cols <- names(df)[sapply(df, function(col) {
    "coe" %in% class(col)
  })]

  # If found, check if multiple
  if (length(coe_cols) > 1) {
    stop(sprintf("Multiple coe columns found: %s. Specify '.cols' to choose which to process.",
                 paste(coe_cols, collapse = ", ")))
  }

  if (length(coe_cols) == 1) {
    return(coe_cols)
  }

  # Otherwise, look for list columns of numeric vectors/matrices
  coe_cols <- names(df)[sapply(df, is_numeric_list)]

  # Check if multiple list columns of numerics
  if (length(coe_cols) > 1) {
    stop(sprintf("Multiple list columns of numeric vectors found: %s. Specify '.cols' to choose which to process.",
                 paste(coe_cols, collapse = ", ")))
  }

  if (length(coe_cols) == 0) {
    stop("No columns of class 'coe' or list columns of numeric vectors found. Specify '.cols' argument explicitly.")
  }

  coe_cols
}

