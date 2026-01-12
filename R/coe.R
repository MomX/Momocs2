# Coefficient Column Detection and Folding/Unfolding Infrastructure
# ===================================================================

#' @importFrom rlang := .data
NULL

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

# fold ----

#' Fold multiple columns into a single list-column
#'
#' Combines multiple related columns (e.g., Fourier coefficients A1, A2, ..., B1, B2, ...)
#' into a single list-column where each row contains a numeric vector of all the values.
#' This is useful for storing morphometric coefficients compactly in a tibble.
#'
#' @param .data A data frame or tibble
#' @param ... A single named argument using tidyselect syntax. The name becomes the
#'   new list-column name, and the selection specifies which columns to fold.
#'   Must be exactly one named argument.
#' @param .class Character vector of classes to add to the list-column and individual
#'   vectors. If `NULL` (default), uses the new column name. For example, if folding
#'   into a column named "coe", both the list-column and individual vectors will get
#'   classes `c("coe", "list")` and `c("coe", "numeric")` respectively.
#'
#' @return A tibble with selected columns removed and replaced by a single list-column
#'   containing classed numeric vectors (one per row). The list-column itself gets
#'   class `c(.class, "list")` and each vector gets `c(.class, "numeric")`.
#'
#' @details
#' The function:
#' 1. Selects columns using tidyselect (e.g., `A1:D5`, `starts_with("harm")`)
#' 2. Converts them to a matrix
#' 3. Splits the matrix by rows to create a list of numeric vectors
#' 4. Preserves original column names as names in each vector
#' 5. Adds appropriate classes to both the list-column and individual vectors
#' 6. Removes the original columns and adds the new list-column
#'
#' This is particularly useful after Fourier analysis where you have many harmonic
#' coefficient columns that you want to store together as a single "coe" object.
#'
#' @seealso [unfold()] for the reverse operation, [get_coe_cols()] for detection
#'
#' @examples
#' # Create sample data with coefficient columns
#' df <- tibble::tibble(
#'   id = 1:3,
#'   A1 = c(1, 2, 3),
#'   A2 = c(4, 5, 6),
#'   B1 = c(7, 8, 9),
#'   B2 = c(10, 11, 12)
#' )
#'
#' # Fold coefficient columns into a single list-column
#' df_folded <- fold(df, coe = A1:B2)
#' df_folded$coe[[1]]
#' #    A1    A2    B1    B2
#' #     1     4     7    10
#'
#' # List-column has classes
#' class(df_folded$coe)
#' # [1] "coe"  "list"
#'
#' # Each element also has classes and preserves names
#' class(df_folded$coe[[1]])
#' # [1] "coe"     "numeric"
#' names(df_folded$coe[[1]])
#' # [1] "A1" "A2" "B1" "B2"
#'
#' # Can specify custom classes (e.g., for efourier)
#' fold(df, coe = A1:B2, .class = c("eft", "coe"))
#'
#' @export
fold <- function(.data, ..., .class = NULL) {
  dots <- rlang::enquos(...)

  if (length(dots) != 1) {
    rlang::abort("Must provide exactly one named argument (e.g., coe = A1:D5)")
  }

  col_name <- names(dots)[1]

  if (is.null(col_name) || col_name == "") {
    rlang::abort("Argument must be named (e.g., coe = A1:D5, not just A1:D5)")
  }

  selection <- dots[[1]]

  cols_to_nest <- tidyselect::eval_select(selection, .data)
  cols_names <- names(cols_to_nest)

  if (length(cols_names) == 0) {
    rlang::abort("No columns selected to fold")
  }

  # Determine classes
  if (is.null(.class)) {
    .class <- col_name
  }

  # Fast: convert to matrix, split by rows
  mat <- as.matrix(.data[cols_names])
  # Preserve column names
  colnames(mat) <- cols_names
  vectors_list <- asplit(mat, 1)

  # Add class to each vector (names are already preserved by asplit)
  vectors_list <- lapply(vectors_list, function(v) {
    class(v) <- c(.class, "numeric")
    v
  })

  # Add class to the list itself
  class(vectors_list) <- c(.class, "list")

  .data %>%
    dplyr::mutate(!!col_name := vectors_list) %>%
    dplyr::select(-dplyr::all_of(cols_names))
}

# unfold ----

#' Unfold a list-column into multiple columns
#'
#' Expands a list-column of numeric vectors into separate columns, one for each
#' element of the vectors. This is the inverse operation of [fold()]. Classes from
#' the list-column are copied to each resulting column. If no column is specified,
#' automatically detects the coe column.
#'
#' @param .data A data frame or tibble
#' @param col Name of the list-column to unfold (unquoted). If missing, automatically
#'   detects the single coe column using [get_coe_cols()].
#' @param .prefix Character string to prefix column names with. Default is to use
#'   the original column name followed by underscore (e.g., "coe_"). Set to `""`
#'   to use original names without prefix, or provide a custom prefix.
#'
#' @return A tibble with the list-column removed and replaced by multiple numeric
#'   columns (one per vector element). Column names are prefixed by default.
#'   Each column inherits classes from the original list-column (e.g., if the
#'   list-column had class `c("eft", "coe")`, each resulting column will too).
#'
#' @details
#' The function:
#' 1. Auto-detects the coe column if not specified
#' 2. Extracts the list-column and its classes
#' 3. Stacks all vectors into a matrix (by rows)
#' 4. Converts the matrix to a tibble with prefixed column names
#' 5. Copies classes from the list-column to each new column
#' 6. Removes the original list-column and binds the new columns
#'
#' This is useful when you need to access individual coefficient values for
#' statistical analysis, plotting, or further manipulation.
#'
#' @seealso [fold()] for the reverse operation, [get_coe_cols()] for detection
#'
#' @examples
#' # Start with folded data
#' df_folded <- tibble::tibble(
#'   id = 1:3,
#'   coe = list(
#'     c(A1 = 1, A2 = 4, B1 = 7, B2 = 10),
#'     c(A1 = 2, A2 = 5, B1 = 8, B2 = 11),
#'     c(A1 = 3, A2 = 6, B1 = 9, B2 = 12)
#'   )
#' )
#' class(df_folded$coe) <- c("eft", "coe", "list")
#'
#' # Auto-detect coe column
#' df_unfolded <- unfold(df_folded)
#' # Or explicit
#' df_unfolded <- unfold(df_folded, coe)
#'
#' # Unfold with default prefix (column name + "_")
#' df_unfolded
#' # A tibble: 3 × 5
#' #      id coe_A1 coe_A2 coe_B1 coe_B2
#' #   <int>  <dbl>  <dbl>  <dbl>  <dbl>
#' # 1     1      1      4      7     10
#'
#' # Each column inherits classes
#' class(df_unfolded$coe_A1)
#' # [1] "eft" "coe"
#'
#' # Unfold without prefix
#' unfold(df_folded, coe, .prefix = "")
#' # Creates: A1, A2, B1, B2
#'
#' # Custom prefix
#' unfold(df_folded, coe, .prefix = "harm_")
#'
#' @export
unfold <- function(.data, col, .prefix = NULL) {
  # Auto-detect column if not provided
  if (missing(col)) {
    col_char <- get_coe_cols(.data)
  } else {
    col_name <- rlang::ensym(col)
    col_char <- rlang::as_name(col_name)

    if (!col_char %in% names(.data)) {
      rlang::abort(sprintf("Column '%s' not found in data", col_char))
    }
  }

  list_col <- .data[[col_char]]

  if (!is.list(list_col)) {
    rlang::abort(sprintf("Column '%s' must be a list-column", col_char))
  }

  if (!all(sapply(list_col, function(x) is.numeric(x) || is.matrix(x)))) {
    rlang::abort(sprintf("All elements in '%s' must be numeric vectors or matrices", col_char))
  }

  # Capture classes from list-column (excluding "list")
  list_classes <- setdiff(class(list_col), "list")

  # Fast: stack vectors into matrix by rows
  mat <- do.call(rbind, list_col)

  # Generate column names if not present
  if (is.null(colnames(mat))) {
    colnames(mat) <- paste0("V", seq_len(ncol(mat)))
  }

  # Determine prefix
  if (is.null(.prefix)) {
    .prefix <- paste0(col_char, "_")
  }

  # Apply prefix if not empty
  if (.prefix != "") {
    colnames(mat) <- paste0(.prefix, colnames(mat))
  }

  # Convert to tibble
  new_cols <- tibble::as_tibble(mat, .name_repair = "minimal")

  # Copy classes to each column (if any)
  if (length(list_classes) > 0) {
    for (col_name in names(new_cols)) {
      class(new_cols[[col_name]]) <- c(list_classes, class(new_cols[[col_name]]))
    }
  }

  .data %>%
    dplyr::select(-!!rlang::sym(col_char)) %>%
    dplyr::bind_cols(new_cols)
}
