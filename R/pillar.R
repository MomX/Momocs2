#' Pillar support for morphometric data types
#'
#' These S3 methods provide custom formatting for morphometric data types
#' when displayed in tibbles via the pillar package.
#'
#' @param x A vector of the appropriate class.
#' @param ... Additional arguments passed to pillar methods.
#'
#' @return
#' * `type_sum.*`: A character string with the abbreviated type name
#' * `pillar_shaft.*`: A pillar shaft object for formatted display
#'
#' @details
#' These methods are called automatically by tibble/pillar and are not
#' intended for direct use. They provide compact, colored display of:
#' * `coo`: coordinate matrices with point count and dimensionality
#' * `out`: outline coordinates with point count
#' * `ldk`: landmark coordinates with point count
#' * `ldk_id`: landmark IDs with count
#' * `opoly`: open polygon coordinates with point count
#' * `npoly`: closed polygon lists with polygon count
#' * `path`: path coordinates with point count
#' * `cur`: curve coordinates with point count
#' * `dct`: discrete cosine transform coefficients with range
#' * `eft`: elliptical Fourier transform coefficients with range
#' * `proc`: Procrustes-aligned coordinates with point count
#'
#' @name pillar_methods
#' @keywords internal
NULL

# coo ----

#' @rdname pillar_methods
#' @export
type_sum.coo <- function(x) {
  "coo"
}

#' @rdname pillar_methods
#' @export
pillar_shaft.coo <- function(x, ...) {
  formatted <- vapply(x, function(coords) {
    if (is.null(coords) || length(coords) == 0) {
      cli::col_grey("(empty)")
    } else {
      n_points <- nrow(coords)
      n_dims <- ncol(coords)
      color_fn <- if (n_points < 50) cli::col_yellow
      else if (n_points < 100) cli::col_green
      else cli::col_blue

      color_fn(sprintf("%d pts %sx %dD", n_points, "x", n_dims))
    }
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}


# out ----

#' @rdname pillar_methods
#' @export
type_sum.out <- function(x) {
  "out"
}

#' @rdname pillar_methods
#' @export
pillar_shaft.out <- function(x, ...) {
  formatted <- vapply(x, function(coords) {
    if (is.null(coords) || length(coords) == 0) {
      cli::col_grey("(empty)")
    } else {
      n_points <- nrow(coords)
      cli::col_green(sprintf("%d pts", n_points))
    }
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}


# ldk ----

#' @rdname pillar_methods
#' @export
type_sum.ldk <- function(x) {
  "ldk"
}

#' @rdname pillar_methods
#' @export
pillar_shaft.ldk <- function(x, ...) {
  formatted <- vapply(x, function(coords) {
    if (is.null(coords) || length(coords) == 0) {
      cli::col_grey("(empty)")
    } else {
      n_points <- nrow(coords)
      cli::col_cyan(sprintf("%d pts", n_points))
    }
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}


# ldk_id ----

#' @rdname pillar_methods
#' @export
type_sum.ldk_id <- function(x) {
  "ldk_id"
}

#' @rdname pillar_methods
#' @export
pillar_shaft.ldk_id <- function(x, ...) {
  formatted <- vapply(x, function(ids) {
    if (is.null(ids) || length(ids) == 0) {
      cli::col_grey("(none)")
    } else {
      n_ids <- length(ids)
      cli::col_magenta(sprintf("%d ids", n_ids))
    }
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}


# opoly ----

#' @rdname pillar_methods
#' @export
type_sum.opoly <- function(x) {
  "opoly"
}

#' @rdname pillar_methods
#' @export
pillar_shaft.opoly <- function(x, ...) {
  formatted <- vapply(x, function(coords) {
    if (is.null(coords) || length(coords) == 0) {
      cli::col_grey("(empty)")
    } else {
      n_points <- nrow(coords)
      cli::col_yellow(sprintf("%d pts", n_points))
    }
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}


# npoly ----

#' @rdname pillar_methods
#' @export
type_sum.npoly <- function(x) {
  "npoly"
}

#' @rdname pillar_methods
#' @export
pillar_shaft.npoly <- function(x, ...) {
  formatted <- vapply(x, function(polys) {
    if (is.null(polys) || length(polys) == 0) {
      cli::col_grey("(none)")
    } else {
      n_polys <- length(polys)
      cli::col_blue(sprintf("%d polys", n_polys))
    }
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}


# path ----

#' @rdname pillar_methods
#' @export
type_sum.path <- function(x) {
  "path"
}

#' @rdname pillar_methods
#' @export
pillar_shaft.path <- function(x, ...) {
  formatted <- vapply(x, function(coords) {
    if (is.null(coords) || length(coords) == 0) {
      cli::col_grey("(empty)")
    } else {
      n_points <- nrow(coords)
      cli::col_green(sprintf("%d pts", n_points))
    }
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}


# cur ----

#' @rdname pillar_methods
#' @export
type_sum.cur <- function(x) {
  "cur"
}

#' @rdname pillar_methods
#' @export
pillar_shaft.cur <- function(x, ...) {
  formatted <- vapply(x, function(coords) {
    if (is.null(coords) || length(coords) == 0) {
      cli::col_grey("(empty)")
    } else {
      n_points <- nrow(coords)
      cli::col_cyan(sprintf("%d pts", n_points))
    }
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}


# dct ----

#' @rdname pillar_methods
#' @export
type_sum.dct <- function(x) {
  "dct"
}

#' @rdname pillar_methods
#' @export
pillar_shaft.dct <- function(x, ...) {
  formatted <- vapply(x, function(coefs) {
    if (is.null(coefs) || length(coefs) == 0) {
      cli::col_grey("(none)")
    } else {
      n_coefs <- length(coefs)
      range_str <- sprintf("[%.1f...%.1f]", min(coefs), max(coefs))
      cli::col_blue(sprintf("%d coefs %s", n_coefs, range_str))
    }
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}


# eft ----

#' @rdname pillar_methods
#' @export
type_sum.eft <- function(x) {
  "eft"
}

#' @rdname pillar_methods
#' @export
pillar_shaft.eft <- function(x, ...) {
  formatted <- vapply(x, function(coefs) {
    if (is.null(coefs) || length(coefs) == 0) {
      cli::col_grey("(none)")
    } else {
      n_coefs <- length(coefs)
      range_str <- sprintf("[%.1f...%.1f]", min(coefs), max(coefs))
      cli::col_magenta(sprintf("%d coefs %s", n_coefs, range_str))
    }
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}


# proc ----

#' @rdname pillar_methods
#' @export
type_sum.proc <- function(x) {
  "proc"
}

#' @rdname pillar_methods
#' @export
pillar_shaft.proc <- function(x, ...) {
  formatted <- vapply(x, function(coords) {
    if (is.null(coords) || length(coords) == 0) {
      cli::col_grey("(empty)")
    } else {
      n_points <- nrow(coords)
      cli::col_green(sprintf("%d pts (aligned)", n_points))
    }
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}
