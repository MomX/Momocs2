# as_ ------

## as_class ---
#' Coerce to Momocs class
#'
#' Add Momocs class attributes to an object while preserving existing classes.
#'
#' Mostly cosmetic effects for various printers.
#'
#' @param x An object (typically a matrix or list).
#' @param class Character or character vector. Class(es) to add.
#'
#' @return The object `x` with updated class attribute.
#'
#' @details
#' Adds new class(es) to the front of the class vector, avoiding duplicates.
#' This allows objects to have multiple class inheritance for proper S3 dispatch.
#'
#' @examples
#' mat <- matrix(rnorm(100), ncol = 2)
#' as_coo(mat)
#' class(as_coo(mat))
#'
#' @keywords internal
#' @noRd
as_class <- function(x, class) {
  class(x) <- unique(c(class, class(x)))
  x
}


## as_coo ----

#' Coerce to Momocs morphometric classes
#'
#' Add class attributes to objects for proper S3 dispatch and printing in
#' morphometric workflows.
#'
#' @param x An object to coerce (typically a matrix, vector, or list).
#'
#' @return Object with updated class attribute.
#'
#' @details
#' These functions add Momocs-specific class attributes that enable:
#' * Custom printing methods
#' * Pretty display in tibbles (via pillar)
#' * S3 method dispatch for morphometric operations
#' * Type checking and validation
#'
#' ## Coordinate classes (inherit from `"coo"`)
#'
#' Used for shape coordinate data, typically stored as nx2 matrices or
#' list-columns of matrices:
#'
#' * **`as_coo()`**: Generic coordinate object - base class for all coordinate types
#' * **`as_out()`**: Closed outlines - class `c("out", "coo")`. For shapes where
#'   the first and last points connect (e.g., leaf outlines, bottle silhouettes)
#' * **`as_ldk()`**: Landmarks - class `c("ldk", "coo")`. For discrete anatomical
#'   points (e.g., skull landmarks, wing vein intersections)
#' * **`as_cur()`**: Open curves - class `c("cur", "coo")`. For shapes with distinct
#'   start and end points (e.g., leaf midribs, antenna segments)
#'
#' ## Single coordinate matrix
#'
#' * **`as_xy()`**: Single coordinate matrix - class `"xy"`. Used for individual
#'   shape matrices (not list-columns). Has custom print method showing dimensions
#'
#' ## Landmark identifiers
#'
#' * **`as_ldk_id()`**: Landmark indices - class `"ldk_id"`. Integer vectors
#'   indicating which points are landmarks. Does NOT inherit from `"coo"` since
#'   it contains indices, not coordinates
#'
#' ## Path metadata
#'
#' * **`as_path()`**: File paths - class `"path"`. Character vectors of image file
#'   paths, typically used to track source images for shapes
#'
#' ## Measurement data
#'
#' * **`as_meas()`**: Measurements - class `"meas"`. Numeric vectors of shape
#'   measurements (area, perimeter, etc.)
#'
#' ## Coefficient classes (all inherit from `"coe"`)
#'
#' Used for shape descriptors from various decomposition methods:
#'
#' * **`as_coe()`**: Generic coefficient object - class `"coe"`. Base class for
#'   all coefficient types
#' * **`as_eft()`**: Elliptic Fourier coefficients - class `c("eft", "coe")`.
#'   From closed outline decomposition (outlines)
#' * **`as_rft()`**: Radii variation Fourier coefficients - class `c("rft", "coe")`.
#'   From radii-based decomposition (outlines)
#' * **`as_dct()`**: Discrete Cosine Transform coefficients - class `c("dct", "coe")`.
#'   From open curve decomposition (curves)
#' * **`as_npoly()`**: Natural polynomial coefficients - class `c("npoly", "coe")`.
#'   From polynomial fitting (curves)
#' * **`as_opoly()`**: Orthogonal polynomial coefficients - class `c("opoly", "coe")`.
#'   From orthogonal polynomial fitting (curves)
#' * **`as_proc()`**: Procrustes coefficients - class `c("proc", "coe")`.
#'   From Procrustes superimposition (landmarks)
#'
#' ## Class hierarchy
#'
#' ```
#' coo (coordinates)
#' ├── out (closed outlines)
#' ├── ldk (landmarks)
#' └── cur (open curves)
#'
#' coe (coefficients)
#' ├── eft (elliptic Fourier - outlines)
#' ├── rft (radii Fourier - outlines)
#' ├── dct (discrete cosine - curves)
#' ├── npoly (natural polynomial - curves)
#' ├── opoly (orthogonal polynomial - curves)
#' └── proc (Procrustes - landmarks)
#'
#' xy (single coordinate matrix)
#' ldk_id (landmark indices)
#' path (file paths)
#' meas (measurements)
#' ```
#'
#' @examples
#' # Coordinate classes
#' mat <- matrix(rnorm(100), ncol = 2)
#' coo <- as_coo(mat)
#' class(coo)  # "coo" "matrix" "array"
#'
#' outline <- as_out(mat)
#' class(outline)  # "out" "coo" "matrix" "array"
#'
#' landmarks <- as_ldk(mat)
#' class(landmarks)  # "ldk" "coo" "matrix" "array"
#'
#' # Coefficient classes
#' coefs <- rnorm(24)  # 6 harmonics × 4 coefficients
#' eft_coefs <- as_eft(coefs)
#' class(eft_coefs)  # "eft" "coe" "numeric"
#'
#' # Landmark identifiers (NOT coo)
#' ldk_indices <- as_ldk_id(c(1, 5, 10, 25))
#' class(ldk_indices)  # "ldk_id" "integer"
#'
#' # In tibbles
#' library(dplyr)
#' tibble(
#'   shape = list(mat, mat * 2),
#'   coef = list(coefs, coefs * 2)
#' ) %>%
#'   mutate(
#'     shape = as_out(shape),
#'     coef = as_eft(coef)
#'   )
#'
#' @seealso [declass()] to remove Momocs classes
#'
#' @name as_class
#' @export
as_coo <- function(x) as_class(x, "coo")

#' @rdname as_class
#' @export
as_out <- function(x) as_class(x, c("out", "coo"))

#' @rdname as_class
#' @export
as_ldk <- function(x) as_class(x, c("ldk", "coo"))

#' @rdname as_class
#' @export
as_xy <- function(x) as_class(x, "xy")

#' @rdname as_class
#' @export
as_ldk_id <- function(x) as_class(x, "ldk_id")

#' @rdname as_class
#' @export
as_cur <- function(x) as_class(x, c("cur", "coo"))

#' @rdname as_class
#' @export
as_path <- function(x) as_class(x, "path")

#' @rdname as_class
#' @export
as_meas <- function(x) as_class(x, "meas")

#' @rdname as_class
#' @export
as_coe <- function(x) as_class(x, "coe")

#' @rdname as_class
#' @export
as_eft <- function(x) as_class(x, c("eft", "coe"))

#' @rdname as_class
#' @export
as_rft <- function(x) as_class(x, c("rft", "coe"))

#' @rdname as_class
#' @export
as_dct <- function(x) as_class(x, c("dct", "coe"))

#' @rdname as_class
#' @export
as_npoly <- function(x) as_class(x, c("npoly", "coe"))

#' @rdname as_class
#' @export
as_opoly <- function(x) as_class(x, c("opoly", "coe"))

#' @rdname as_class
#' @export
as_proc <- function(x) as_class(x, c("proc", "coe"))


# printers ----------

#' @export
print.xy <- function(x, n = 5, digits = 3, ...) {
  # Header
  cat(sprintf("<xy [%d x %d]>\n", nrow(x), ncol(x)))

  # Remove row names
  x_no_rownames <- x
  rownames(x_no_rownames) <- NULL

  # If n is large enough to show all rows, print everything
  if (nrow(x_no_rownames) <= 2 * n) {
    # Round and format with right alignment
    x_rounded <- round(x_no_rownames, digits = digits)
    print(format(x_rounded, justify = "right"), quote = FALSE)
    return(invisible(x))
  }

  # Otherwise: n top + "... ..." + n bottom
  # Round first, then extract top/bottom
  x_rounded <- round(x_no_rownames, digits = digits)
  top <- head(x_rounded, n)
  bottom <- tail(x_rounded, n)

  # Create separator row with dots (as character)
  sep <- matrix(c("...", "..."), nrow = 1)
  colnames(sep) <- colnames(x_rounded)

  # Combine - need to convert numeric to character for rbind
  combined <- rbind(
    format(top, justify = "right"),
    format(sep, justify = "right"),
    format(bottom, justify = "right")
  )
  rownames(combined) <- NULL

  print(combined, quote = FALSE)

  invisible(x)
}


# pillars ----------

#' Pillar support for morphometric data types
#'
#' These functions provide custom display methods for morphometric classes
#' in tibble printing via the pillar package.
#'
#' @name pillar_methods
#' @keywords internal
NULL


# type_sum methods ----

#' @rdname pillar_methods
#' @export
type_sum.coo <- function(x) {
  cli::style_bold(cli::col_blue("coo"))
}

#' @rdname pillar_methods
#' @export
type_sum.out <- function(x) {
  cli::style_bold(cli::col_blue("out"))
}

#' @rdname pillar_methods
#' @export
type_sum.ldk <- function(x) {
  cli::style_bold(cli::col_red("ldk"))
}

#' @rdname pillar_methods
#' @export
type_sum.ldk_id <- function(x) {
  cli::style_bold(cli::col_red("ldk_id"))
}

#' @rdname pillar_methods
#' @export
type_sum.path <- function(x) {
  cli::style_bold(cli::col_silver("path"))
}

#' @rdname pillar_methods
#' @export
type_sum.cur <- function(x) {
  cli::style_bold(cli::col_cyan("cur"))
}

#' @rdname pillar_methods
#' @export
type_sum.dct <- function(x) {
  cli::style_bold(cli::col_cyan("dct"))
}

#' @rdname pillar_methods
#' @export
type_sum.eft <- function(x) {
  cli::style_bold(cli::col_cyan("eft"))
}

#' @rdname pillar_methods
#' @export
type_sum.npoly <- function(x) {
  cli::style_bold(cli::col_cyan("npoly"))
}

#' @rdname pillar_methods
#' @export
type_sum.opoly <- function(x) {
  cli::style_bold(cli::col_cyan("opoly"))
}

#' @rdname pillar_methods
#' @export
type_sum.proc <- function(x) {
  cli::style_bold(cli::col_green("proc"))
}


# pillar_shaft methods ----

#' @rdname pillar_methods
#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @importFrom cli col_blue col_grey col_red col_cyan col_green col_silver style_bold
#' @export
pillar_shaft.coo <- function(x, ...) {
  formatted <- vapply(x, function(coords) {
    if (!is.matrix(coords)) return(cli::col_grey("<NA>"))
    n_points <- nrow(coords)
    cli::col_blue(sprintf("(%d x 2)", n_points))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.out <- function(x, ...) {
  formatted <- vapply(x, function(coords) {
    if (!is.matrix(coords)) return(cli::col_grey("<NA>"))
    n_points <- nrow(coords)
    cli::col_blue(sprintf("(%d x 2)", n_points))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.ldk <- function(x, ...) {
  formatted <- vapply(x, function(coords) {
    if (!is.matrix(coords)) return(cli::col_grey("<NA>"))
    n_ldk <- nrow(coords)
    cli::col_red(sprintf("[%d x 2]", n_ldk))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.ldk_id <- function(x, ...) {
  formatted <- vapply(x, function(ids) {
    if (is.null(ids) || length(ids) == 0) return(cli::col_grey("<NA>"))
    n_ldk <- length(ids)
    cli::col_red(sprintf("[%d]", n_ldk))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.path <- function(x, ...) {
  formatted <- vapply(x, function(p) {
    if (is.na(p)) return(cli::col_grey("<NA>"))
    # Show just filename, not full path
    cli::col_silver(basename(p))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.cur <- function(x, ...) {
  formatted <- vapply(x, function(coords) {
    if (!is.matrix(coords)) return(cli::col_grey("<NA>"))
    n_points <- nrow(coords)
    cli::col_cyan(sprintf("(%d x 2)", n_points))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.dct <- function(x, ...) {
  formatted <- vapply(x, function(coeffs) {
    if (!is.numeric(coeffs)) return(cli::col_grey("<NA>"))
    n_coeffs <- length(coeffs)
    cli::col_cyan(sprintf("%d dct", n_coeffs))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.eft <- function(x, ...) {
  formatted <- vapply(x, function(coeffs) {
    if (length(coeffs) == 1 && is.na(coeffs)) {
      return(cli::col_grey("<NA>"))
    }
    n_harmonics <- length(coeffs) / 4
    cli::col_cyan(sprintf("<%gh x 4>", n_harmonics))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.npoly <- function(x, ...) {
  formatted <- vapply(x, function(coeffs) {
    if (!is.numeric(coeffs)) return(cli::col_grey("<NA>"))
    n_coeffs <- length(coeffs)
    cli::col_cyan(sprintf("%d np", n_coeffs))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.opoly <- function(x, ...) {
  formatted <- vapply(x, function(coeffs) {
    if (!is.numeric(coeffs)) return(cli::col_grey("<NA>"))
    n_coeffs <- length(coeffs)
    cli::col_cyan(sprintf("%d op", n_coeffs))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.proc <- function(x, ...) {
  formatted <- vapply(x, function(coords) {
    if (!is.matrix(coords)) return(cli::col_grey("<NA>"))
    n_ldk <- nrow(coords)
    cli::col_green(sprintf("%d proc", n_ldk))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}


# Class helpers ------

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
