# misc ------

#' Relocate morphometric columns to front
#'
#' Reorder columns in a tibble so that morphometric data columns appear first
#' in a logical order: path columns, then coo columns, then coe columns,
#' followed by all other columns.
#'
#' @param .data A tibble or data frame
#'
#' @return The same tibble with columns reordered
#'
#' @details
#' This is a convenience function to organize morphometric data in a consistent,
#' readable order. The ordering priority is:
#' 1. **path** columns (image file paths) - typically named "path" or containing "path"
#' 2. **coo** columns (coordinates) - list-columns with class "coo"
#' 3. **coe** columns (coefficients) - list-columns with class "coe"
#' 4. **everything else** - metadata, grouping variables, etc.
#'
#' If multiple columns of the same type exist, their relative order is preserved.
#'
#' @examples
#' \dontrun{
#' # After adding coefficients, coe column is at the end
#' bot %>% efourier()
#'
#' # Relocate to put coe after coo
#' bot %>% efourier() %>% front()
#'
#' # Works with any combination
#' tibble(
#'   id = 1:3,
#'   species = c("A", "B", "C"),
#'   coe = list(1:24, 1:24, 1:24),
#'   coo = list(matrix(1:10, ncol=2), matrix(1:10, ncol=2), matrix(1:10, ncol=2)),
#'   path = c("img1.jpg", "img2.jpg", "img3.jpg")
#' ) %>% front()
#' # Result: path, coo, coe, id, species
#' }
#'
#' @export
front <- function(.data) {
  if (!is.data.frame(.data)) {
    stop(".data must be a data frame or tibble")
  }

  # Find path columns (containing "path" in name, case-insensitive)
  path_cols <- names(.data)[grepl("path", names(.data), ignore.case = TRUE)]

  # Find coo columns (have "coo" class)
  coo_cols <- names(.data)[vapply(.data, function(col) {
    "coo" %in% class(col)
  }, logical(1))]

  # Find coe columns (have "coe" class)
  coe_cols <- names(.data)[vapply(.data, function(col) {
    "coe" %in% class(col)
  }, logical(1))]

  # Get all other columns
  morpho_cols <- c(path_cols, coo_cols, coe_cols)
  other_cols <- setdiff(names(.data), morpho_cols)

  # Relocate: path, coo, coe, then everything else
  dplyr::select(.data,
                dplyr::any_of(path_cols),
                dplyr::any_of(coo_cols),
                dplyr::any_of(coe_cols),
                dplyr::all_of(other_cols))
}


#' Remove Momocs classes
#'
#' Strip all Momocs-specific class attributes, retaining only base R classes.
#'
#' @param x An object with Momocs classes.
#'
#' @return Object with Momocs classes removed, retaining base R classes
#'   (e.g., "matrix", "numeric", "list", "data.frame").
#'
#' @details
#' Removes all Momocs morphometric classes while preserving base R structure:
#' * Coordinate classes: `coo`, `out`, `ldk`, `cur`, `xy`
#' * Coefficient classes: `coe`, `eft`, `rft`, `dct`, `npoly`, `opoly`, `proc`
#' * Other classes: `ldk_id`, `path`, `meas`
#'
#' This is useful when:
#' * Exporting data to other packages that don't recognize Momocs classes
#' * Debugging class-related issues
#' * Converting back to plain R objects for generic operations
#'
#' The function works recursively on list-columns in data frames.
#'
#' @examples
#' # Single object
#' mat <- matrix(rnorm(100), ncol = 2)
#' outline <- as_out(mat)
#' class(outline)
#' # [1] "out"    "coo"    "matrix" "array"
#'
#' plain <- declass(outline)
#' class(plain)
#' # [1] "matrix" "array"
#'
#' # Coefficient vector
#' coefs <- as_eft(rnorm(24))
#' class(coefs)
#' # [1] "eft"     "coe"     "numeric"
#'
#' plain_coefs <- declass(coefs)
#' class(plain_coefs)
#' # [1] "numeric"
#'
#' \dontrun{
#' # Data frame with list-columns
#' library(dplyr)
#' df <- tibble(
#'   id = 1:3,
#'   shape = list(mat, mat, mat),
#'   coef = list(coefs, coefs, coefs)
#' ) %>%
#'   mutate(
#'     shape = as_out(shape),
#'     coef = as_eft(coef)
#'   )
#'
#' class(df$shape)
#' # [1] "out" "coo" "list"
#'
#' df_plain <- declass(df)
#' class(df_plain$shape)
#' # [1] "list"
#' }
#'
#' @seealso [as_class] for adding Momocs classes
#'
#' @export
declass <- function(x) {
  # Define all Momocs classes to remove
  momocs_classes <- c(
    # Coordinate classes
    "coo", "out", "ldk", "cur", "xy",
    # Coefficient classes
    "coe", "eft", "rft", "dct", "npoly", "opoly", "proc",
    # Other classes
    "ldk_id", "path", "meas"
  )

  # If it's a data frame, process each column
  if (is.data.frame(x)) {
    for (col in names(x)) {
      x[[col]] <- declass(x[[col]])
    }
    return(x)
  }

  # If it's a list (but not a data frame), process each element
  if (is.list(x)) {
    x <- lapply(x, declass)
    # Remove Momocs classes from the list itself
    class(x) <- setdiff(class(x), momocs_classes)
    return(x)
  }

  # For atomic objects, just remove Momocs classes
  class(x) <- setdiff(class(x), momocs_classes)

  x
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
#'   vectors. If `NULL` (default), no classes are added - the list-column remains
#'   a plain list. For morphometric coefficients, specify classes explicitly, e.g.,
#'   `.class = c("eft", "coe")` or `.class = "coe"`.
#'
#' @return A tibble with selected columns removed and replaced by a single list-column
#'   containing numeric vectors (one per row). If `.class` is provided, both the
#'   list-column and individual vectors get those classes (e.g., `c("eft", "coe", "list")`
#'   and `c("eft", "coe", "numeric")`). If `.class = NULL`, returns a plain list-column.
#'
#' @details
#' The function:
#' 1. Selects columns using tidyselect (e.g., `A1:D5`, `starts_with("harm")`)
#' 2. Converts them to a matrix
#' 3. Splits the matrix by rows to create a list of numeric vectors
#' 4. Preserves original column names as names in each vector
#' 5. If `.class` is provided, adds classes to both the list-column and individual vectors
#' 6. Removes the original columns and adds the new list-column
#'
#' This is particularly useful after Fourier analysis where you have many harmonic
#' coefficient columns that you want to store together. For morphometric coefficients,
#' always specify `.class` explicitly to ensure proper class attributes.
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
#' df_folded <- fold(df, coe = A1:B2)  # Plain list, no classes
#' df_folded$coe[[1]]
#' #    A1    A2    B1    B2
#' #     1     4     7    10
#'
#' # For morphometric coefficients, specify classes explicitly
#' df_eft <- fold(df, coe = A1:B2, .class = c("eft", "coe"))
#'
#' # List-column has classes only if specified
#' class(df_eft$coe)
#' # [1] "eft"  "coe"  "list"
#'
#' # Each element also has classes
#' class(df_eft$coe[[1]])
#' # [1] "eft"     "coe"     "numeric"
#' names(df_eft$coe[[1]])
#' # [1] "A1" "A2" "B1" "B2"
#'
#' # Without .class, just a plain list
#' df_plain <- fold(df, scores = A1:B2)
#' class(df_plain$scores)
#' # [1] "list"
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

  # Fast: convert to matrix, split by rows
  mat <- as.matrix(.data[cols_names])
  # Preserve column names
  colnames(mat) <- cols_names
  vectors_list <- asplit(mat, 1)

  # Only add classes if explicitly provided
  if (!is.null(.class)) {
    # Add class to each vector (names are already preserved by asplit)
    vectors_list <- lapply(vectors_list, function(v) {
      class(v) <- c(.class, "numeric")
      v
    })

    # Add class to the list itself
    class(vectors_list) <- c(.class, "list")
  }

  .data %>%
    dplyr::mutate(!!col_name := vectors_list) %>%
    dplyr::select(-dplyr::all_of(cols_names))
}

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

# wrap --------

#' Wrap coordinates from rows to list-column of matrices
#'
#' Converts x and y coordinate columns into a list-column of matrices, with one
#' matrix per group (defined by other columns or an `.id` column).
#'
#' @param .data A tibble with x and y coordinate columns
#' @param x Column name for x coordinates (unquoted). Default is `x`.
#' @param y Column name for y coordinates (unquoted). Default is `y`.
#' @param .into Name of the new list-column to create. Default is `"coo"`.
#' @param .class Character vector of classes to PREFIX to the list-column.
#'   The list-column always has base classes `c("coo", "list")`. If `.class` is
#'   provided, these classes are prepended, e.g., `.class = "out"` results in
#'   `c("out", "coo", "list")`. Individual matrices always get class
#'   `c("xy", "matrix", "array")` regardless of this parameter. Set to `NULL`
#'   for just `c("coo", "list")`.
#'
#' @return A tibble with x and y columns removed and replaced by a list-column
#'   containing coordinate matrices (one per group). Individual matrices have
#'   class `c("xy", "matrix", "array")` with column names `c("x", "y")`.
#'   The list-column has class `c(.class, "coo", "list")` if `.class` is provided,
#'   or `c("coo", "list")` if `.class = NULL`.
#'
#' @details
#' The function groups rows by all columns except x and y, then wraps each group's
#' coordinates into a matrix. This is the inverse of [unwrap()].
#'
#' Grouping behavior:
#' * If `.data` has grouping (from `group_by()`), uses existing groups
#' * If no groups, wraps ALL rows into a single matrix (assumes single shape)
#' * For multiple shapes, ensure you have identifying columns or use `.id` in [unwrap()]
#'
#' @seealso [unwrap()] for the reverse operation
#'
#' @examples
#' # Start with unwrapped coordinates
#' df_unwrapped <- tibble::tibble(
#'   id = rep(1:3, each = 4),
#'   x = c(0, 1, 1, 0,  2, 3, 3, 2,  4, 5, 5, 4),
#'   y = c(0, 0, 1, 1,  0, 0, 1, 1,  0, 0, 1, 1)
#' )
#'
#' # Wrap by id (x and y are defaults)
#' df_wrapped <- df_unwrapped %>% wrap()
#' df_wrapped
#' # A tibble: 3 × 2
#' #      id coo
#' #   <int> <coo>
#' # 1     1 (4 x 2)
#' # 2     2 (4 x 2)
#' # 3     3 (4 x 2)
#'
#' # Explicit column names
#' df_wrapped <- df_unwrapped %>% wrap(x, y, .into = "coo")
#'
#' # Individual matrices have "xy" class
#' df_wrapped$coo[[1]]
#' class(df_wrapped$coo[[1]])
#' # [1] "xy"     "matrix" "array"
#'
#' # List-column has "coo" class
#' class(df_wrapped$coo)
#' # [1] "coo"  "list"
#'
#' # Prefix with "out" for outlines
#' df_out <- wrap(df_unwrapped, .into = "outline", .class = "out")
#' class(df_out$outline)
#' # [1] "out"  "coo"  "list"
#'
#' # No prefix classes (just base "coo")
#' wrap(df_unwrapped, .class = NULL)
#'
#' @export
wrap <- function(.data, x = x, y = y, .into = "coo", .class = "coo") {

  x_col <- rlang::ensym(x)
  y_col <- rlang::ensym(y)

  # Check columns exist
  if (!rlang::as_name(x_col) %in% names(.data)) {
    stop(sprintf("Column '%s' not found", rlang::as_name(x_col)))
  }
  if (!rlang::as_name(y_col) %in% names(.data)) {
    stop(sprintf("Column '%s' not found", rlang::as_name(y_col)))
  }

  # Check if .into already exists
  if (.into %in% names(.data)) {
    stop(sprintf("Column '%s' already exists", .into))
  }

  # Get grouping columns (everything except x and y)
  coord_cols <- c(rlang::as_name(x_col), rlang::as_name(y_col))
  group_cols <- setdiff(names(.data), coord_cols)

  # If no grouping columns, assume single shape
  if (length(group_cols) == 0) {
    # Single matrix for all rows
    mat <- as.matrix(.data[coord_cols])
    colnames(mat) <- c("x", "y")

    # Individual matrix always gets "xy" class
    class(mat) <- c("xy", "matrix", "array")

    result <- tibble::tibble(!!.into := list(mat))

    # List-column gets .class (if provided) + "coo" + "list"
    if (!is.null(.class)) {
      class(result[[.into]]) <- c(.class, "coo", "list")
    } else {
      class(result[[.into]]) <- c("coo", "list")
    }

    return(result)
  }

  # Group by all non-coordinate columns and nest
  result <- .data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      !!.into := list({
        mat <- cbind(!!x_col, !!y_col)
        colnames(mat) <- c("x", "y")
        # Individual matrices always get "xy" class
        class(mat) <- c("xy", "matrix", "array")
        mat
      }),
      .groups = "drop"
    )

  # List-column gets .class (if provided) + "coo" + "list"
  if (!is.null(.class)) {
    class(result[[.into]]) <- c(.class, "coo", "list")
  } else {
    class(result[[.into]]) <- c("coo", "list")
  }

  result
}


#' Unwrap coordinates from list-column of matrices to rows
#'
#' Converts a list-column of coordinate matrices into separate x and y columns,
#' with one row per coordinate point.
#'
#' @param .data A tibble with a list-column containing coordinate matrices
#' @param col Column name of the list-column to unwrap (unquoted). If missing,
#'   auto-detects the `coo` column.
#' @param .id Optional name for an ID column to identify which matrix each row
#'   came from. If `NULL` (default) and no other identifying columns exist,
#'   adds a column named `".id"` automatically. Set to `FALSE` to never add
#'   an ID column.
#'
#' @return A tibble with the list-column removed and replaced by `x` and `y`
#'   columns (one row per coordinate point). An ID column is added if needed
#'   to track which matrix each row belongs to.
#'
#' @details
#' The function converts each matrix in the list-column to a tibble with x and y
#' columns, then unnests to create one row per coordinate point. This is the
#' inverse of [wrap()].
#'
#' ID column behavior:
#' * If `.data` has other columns besides the list-column: uses those for identification
#' * If `.data` has ONLY the list-column: adds `.id` column automatically (or uses custom name)
#' * Set `.id = FALSE` to never add an ID column (may create ambiguous data)
#'
#' @seealso [wrap()] for the reverse operation
#'
#' @examples
#'
#' bot %>% unwrap()
#'
#' bot %>% unwrap() %>% wrap()
#'
#' @export
unwrap <- function(.data, col, .id = NULL) {

  # Auto-detect column if not provided
  if (missing(col)) {
    coo_cols <- names(.data)[vapply(.data, function(x) "coo" %in% class(x), logical(1))]

    if (length(coo_cols) == 0) {
      stop("No 'coo' column found. Please specify the column explicitly.")
    }
    if (length(coo_cols) > 1) {
      stop(sprintf("Multiple 'coo' columns found: %s. Please specify which one to unwrap.",
                   paste(coo_cols, collapse = ", ")))
    }

    col_name <- coo_cols[1]
  } else {
    col_sym <- rlang::ensym(col)
    col_name <- rlang::as_name(col_sym)

    if (!col_name %in% names(.data)) {
      stop(sprintf("Column '%s' not found in data", col_name))
    }
  }

  # Check if list-column
  if (!is.list(.data[[col_name]])) {
    stop(sprintf("Column '%s' must be a list-column", col_name))
  }

  # Determine if we need to add an ID column
  other_cols <- setdiff(names(.data), col_name)
  needs_id <- length(other_cols) == 0

  # Handle .id parameter
  add_id <- FALSE
  id_col_name <- NULL

  if (!is.null(.id)) {
    # User explicitly requested an ID column
    if (isFALSE(.id)) {
      # Explicitly don't want ID
      add_id <- FALSE
    } else {
      # User provided a column name
      id_col_name <- as.character(.id)

      # Check if it already exists
      if (id_col_name %in% names(.data)) {
        stop(sprintf("Column '%s' already exists. Choose a different .id name or set .id = FALSE.",
                     id_col_name))
      }

      add_id <- TRUE
    }
  } else if (needs_id) {
    # No other columns and .id not specified - add default ".id"
    id_col_name <- ".id"

    # Check if .id already exists (unlikely but possible)
    if (id_col_name %in% names(.data)) {
      stop("Column '.id' already exists. Specify a custom .id name or set .id = FALSE.")
    }

    add_id <- TRUE
  }

  # Add ID column if needed
  if (add_id) {
    .data <- .data %>%
      dplyr::mutate(!!id_col_name := dplyr::row_number(), .before = 1)
  }

  # Convert matrices to tibbles with x, y columns
  .data %>%
    dplyr::mutate(
      !!col_name := lapply(.data[[col_name]], function(m) {
        if (is.matrix(m) && ncol(m) >= 2) {
          # Use first two columns as x, y
          tibble::tibble(x = m[, 1], y = m[, 2])
        } else if (is.matrix(m)) {
          stop("Matrix must have at least 2 columns")
        } else {
          stop("List-column elements must be matrices")
        }
      })
    ) %>%
    tidyr::unnest(!!rlang::sym(col_name))
}
