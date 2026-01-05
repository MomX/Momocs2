# as_class ----

#' Coerce to Momocs class
#'
#' Add Momocs class attributes to an object while preserving existing classes.
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


# as_coo ----

#' Coerce to coordinate classes
#'
#' Convert objects to Momocs coordinate and coefficient classes.
#'
#' @param x An object to coerce.
#'
#' @return Object with updated class attribute.
#'
#' @details
#' **Coordinate classes (all inherit from "coo"):**
#' * `as_coo()`: generic coordinate object
#' * `as_out()`: outline (closed polygon)
#' * `as_ldk()`: landmarks
#' * `as_cur()`: curve (open outline)
#' * `as_path()`: path (open curve with direction)
#' * `as_opoly()`: open polygon
#'
#' **Non-coordinate classes:**
#' * `as_ldk_id()`: landmark identifiers (not inherits from coo)
#'
#' **Coefficient classes (all inherit from "coe"):**
#' * `as_coe()`: generic coefficient object
#' * `as_eft()`: Elliptic Fourier Transform coefficients
#' * `as_dct()`: Discrete Cosine Transform coefficients
#' * `as_proc()`: Procrustes-aligned coefficients
#' * `as_npoly()`: nested polygon coefficients
#'
#' @examples
#' mat <- matrix(rnorm(100), ncol = 2)
#' as_coo(mat)
#' as_out(mat)
#' as_ldk(mat)
#'
#' coefs <- rnorm(20)
#' as_eft(coefs)
#' as_dct(coefs)
#'
#' @rdname as_class
#' @keywords internal
#' @export
as_coo <- function(x) as_class(x, "coo")


# as_out ----

#' @rdname as_class
#' @export
as_out <- function(x) as_class(x, c("out", "coo"))


# as_ldk ----

#' @rdname as_class
#' @export
as_ldk <- function(x) as_class(x, c("ldk", "coo"))


# as_ldk_id ----

#' @rdname as_class
#' @export
as_ldk_id <- function(x) as_class(x, "ldk_id")


# as_cur ----

#' @rdname as_class
#' @export
as_cur <- function(x) as_class(x, c("cur", "coo"))


# as_path ----

#' @rdname as_class
#' @export
as_path <- function(x) as_class(x, "path")


# as_opoly ----

#' @rdname as_class
#' @export
as_opoly <- function(x) as_class(x, c("opoly", "coe"))

# as_meas ----
as_meas <- function(x) as_class(x, "meas")

# as_coe ----

#' @rdname as_class
#' @export
as_coe <- function(x) as_class(x, "coe")


# as_eft ----

#' @rdname as_class
#' @export
as_eft <- function(x) as_class(x, c("eft", "coe"))


# as_dct ----

#' @rdname as_class
#' @export
as_dct <- function(x) as_class(x, c("dct", "coe"))


# as_npoly ----

#' @rdname as_class
#' @export
as_npoly <- function(x) as_class(x, c("npoly", "coe"))


# as_proc ----

#' @rdname as_class
#' @export
as_proc <- function(x) as_class(x, c("proc", "coe"))
