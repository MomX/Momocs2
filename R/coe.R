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

#' Unfold a list-column into multiple columns or a matrix
#'
#' Expands a list-column of numeric vectors into separate columns (when applied to
#' a tibble) or into a matrix (when applied to a list-column directly). This is the
#' inverse operation of [fold()].
#'
#' @param x A data frame, tibble, or list of numeric vectors
#' @param ... Additional arguments passed to methods
#'
#' @return
#' * For tibbles: A tibble with the list-column removed and replaced by multiple
#'   numeric columns
#' * For lists: A numeric matrix with one row per list element
#'
#' @details
#' `unfold()` is an S3 generic with methods for:
#' * **Data frames/tibbles** (`unfold.data.frame`): User-facing, expands list-columns
#'   into separate columns with prefixes and class preservation
#' * **Lists** (`unfold.list`): Internal use, converts list of vectors directly to
#'   a matrix for statistical functions
#'
#' @seealso [unfold.data.frame()], [unfold.list()], [fold()]
#'
#' @examples
#' # For tibbles - see ?unfold.data.frame
#' df_folded <- tibble::tibble(
#'   id = 1:3,
#'   coe = list(c(A1=1, A2=4), c(A1=2, A2=5), c(A1=3, A2=6))
#' )
#' unfold(df_folded, coe)
#'
#' # For lists directly (returns matrix)
#' coe_list <- list(c(A1=1, A2=4), c(A1=2, A2=5), c(A1=3, A2=6))
#' unfold(coe_list)
#' #      A1 A2
#' # [1,]  1  4
#' # [2,]  2  5
#' # [3,]  3  6
#'
#' @export
unfold <- function(x, ...) {
  UseMethod("unfold")
}


#' @rdname unfold
#' @param col Name of the list-column to unfold (unquoted). If missing, automatically
#'   detects the single coe column using [get_coe_cols()].
#' @param .prefix Character string to prefix column names with. Default is to use
#'   the original column name followed by underscore (e.g., "coe_"). Set to `""`
#'   to use original names without prefix, or provide a custom prefix.
#'
#' @details
#' ## For data frames (tibbles)
#'
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
unfold.data.frame <- function(x, col, .prefix = NULL, ...) {
  # Rename for clarity
  .data <- x

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

  # Convert to tibble (just numeric columns, no special classes)
  new_cols <- tibble::as_tibble(mat, .name_repair = "minimal")

  # Don't copy classes - unfolded columns are just regular numeric values
  # The classes (eft, coe, etc.) apply to the list-column, not individual harmonics

  .data %>%
    dplyr::select(-!!rlang::sym(col_char)) %>%
    dplyr::bind_cols(new_cols)
}


#' @rdname unfold
#'
#' @details
#' ## For lists
#'
#' When applied directly to a list of numeric vectors, `unfold()` converts it to
#' a matrix by stacking vectors as rows. This is useful for internal operations
#' that need matrix input (e.g., `prcomp()`, `lda()`).
#'
#' The function:
#' 1. Finds the first non-NA element to determine structure
#' 2. Extracts or generates column names
#' 3. Stacks all vectors into a matrix (one row per vector)
#' 4. Preserves names as column names
#'
#' @param .prefix Character string to prefix column names. Default `NULL` means no prefix.
#'   Set to a string to add a prefix to all column names.
#'
#' @examples
#' # Direct list to matrix conversion
#' coe_list <- list(
#'   c(A1 = 1, A2 = 4, B1 = 7),
#'   c(A1 = 2, A2 = 5, B1 = 8),
#'   c(A1 = 3, A2 = 6, B1 = 9)
#' )
#'
#' mat <- unfold(coe_list)
#' mat
#' #      A1 A2 B1
#' # [1,]  1  4  7
#' # [2,]  2  5  8
#' # [3,]  3  6  9
#'
#' # With prefix
#' unfold(coe_list, .prefix = "coef_")
#' #      coef_A1 coef_A2 coef_B1
#' # [1,]       1       4       7
#'
#' @export
unfold.list <- function(x, .prefix = NULL, ...) {

  if (!is.list(x)) {
    rlang::abort("x must be a list")
  }

  # Get first non-NA element to determine structure
  first_valid_idx <- which(!vapply(x, function(y) all(is.na(y)), logical(1)))[1]

  if (is.na(first_valid_idx)) {
    rlang::abort("All elements in list are NA")
  }

  first_valid <- x[[first_valid_idx]]

  # Get coefficient names from first valid element or generate
  if (!is.null(names(first_valid))) {
    coef_names <- names(first_valid)
  } else {
    coef_names <- paste0("V", seq_along(first_valid))
  }

  n_coefs <- length(first_valid)
  n_obs <- length(x)

  # Build matrix by stacking vectors
  mat <- matrix(NA_real_, nrow = n_obs, ncol = n_coefs)
  colnames(mat) <- coef_names

  for (i in seq_len(n_obs)) {
    if (!is.null(x[[i]]) && !all(is.na(x[[i]]))) {
      mat[i, ] <- x[[i]]
    }
  }

  # Apply prefix if provided
  if (!is.null(.prefix) && .prefix != "") {
    colnames(mat) <- paste0(.prefix, colnames(mat))
  }

  mat
}
