# Pillar support for morphometric data types ----

#' Pillar support for morphometric data types
#'
#' These functions provide custom display methods for morphometric classes
#' in tibble printing via the pillar package.
#'
#' @name pillar_methods
#' @keywords internal
NULL


# Color helpers ----

#' Get pillar color for a class
#'
#' Retrieves color from options or uses default.
#'
#' @param class_name Character. Name of the class (e.g., "out", "ldk")
#' @param default Character. Default color if option not set
#'
#' @return A cli color function
#' @keywords internal
#' @noRd
get_pillar_color <- function(class_name, default = "blue") {
  opt_name <- paste0("momocs2.pillar.color.", class_name)
  color_name <- getOption(opt_name, default)

  # Get cli color function
  color_fn <- switch(color_name,
                     "blue" = cli::col_blue,
                     "red" = cli::col_red,
                     "green" = cli::col_green,
                     "yellow" = cli::col_yellow,
                     "cyan" = cli::col_cyan,
                     "magenta" = cli::col_magenta,
                     "silver" = cli::col_silver,
                     "grey" = cli::col_grey,
                     cli::col_blue  # fallback
  )

  color_fn
}

#' Get type_sum style (bold + color)
#'
#' @param class_name Character. Name of the class
#' @param default Character. Default color
#'
#' @return A styled string
#' @keywords internal
#' @noRd
style_type_sum <- function(class_name, default = "blue") {
  color_fn <- get_pillar_color(class_name, default)
  function(text) {
    cli::style_bold(color_fn(text))
  }
}


# type_sum methods ----

#' @rdname pillar_methods
#' @export
type_sum.coo <- function(x) {
  style_type_sum("coo")("coo")
}

#' @rdname pillar_methods
#' @export
type_sum.out <- function(x) {
  style_type_sum("out")("out")
}

#' @rdname pillar_methods
#' @export
type_sum.ldk <- function(x) {
  style_type_sum("ldk", "red")("ldk")
}

#' @rdname pillar_methods
#' @export
type_sum.ldk_id <- function(x) {
  style_type_sum("ldk_id", "red")("ldk_id")
}

#' @rdname pillar_methods
#' @export
type_sum.path <- function(x) {
  style_type_sum("path", "silver")("path")
}

#' @rdname pillar_methods
#' @export
type_sum.cur <- function(x) {
  style_type_sum("cur", "cyan")("cur")
}

#' @rdname pillar_methods
#' @export
type_sum.dct <- function(x) {
  style_type_sum("dct", "cyan")("dct")
}

#' @rdname pillar_methods
#' @export
type_sum.eft <- function(x) {
  style_type_sum("eft", "cyan")("eft")
}

#' @rdname pillar_methods
#' @export
type_sum.npoly <- function(x) {
  style_type_sum("npoly", "cyan")("npoly")
}

#' @rdname pillar_methods
#' @export
type_sum.opoly <- function(x) {
  style_type_sum("opoly", "cyan")("opoly")
}

#' @rdname pillar_methods
#' @export
type_sum.proc <- function(x) {
  style_type_sum("proc", "green")("proc")
}


# pillar_shaft methods ----

#' @rdname pillar_methods
#' @importFrom pillar pillar_shaft new_pillar_shaft_simple
#' @importFrom cli col_blue col_grey col_red col_cyan col_green col_silver style_bold
#' @export
pillar_shaft.coo <- function(x, ...) {
  color_fn <- get_pillar_color("coo", "blue")

  formatted <- vapply(x, function(coords) {
    if (!is.matrix(coords)) return(cli::col_grey("<NA>"))
    n_points <- nrow(coords)
    color_fn(sprintf("%d pts", n_points))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.out <- function(x, ...) {
  color_fn <- get_pillar_color("out", "blue")

  formatted <- vapply(x, function(coords) {
    if (!is.matrix(coords)) return(cli::col_grey("<NA>"))
    n_points <- nrow(coords)
    color_fn(sprintf("(%d\u00B72)", n_points))  # (80·2)
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.ldk <- function(x, ...) {
  color_fn <- get_pillar_color("ldk", "red")

  formatted <- vapply(x, function(coords) {
    if (!is.matrix(coords)) return(cli::col_grey("<NA>"))
    n_ldk <- nrow(coords)
    color_fn(sprintf("[%d\u00B72]", n_ldk))  # [4·2]
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.ldk_id <- function(x, ...) {
  color_fn <- get_pillar_color("ldk_id", "red")

  formatted <- vapply(x, function(ids) {
    if (is.null(ids) || length(ids) == 0) return(cli::col_grey("<NA>"))
    n_ldk <- length(ids)
    color_fn(sprintf("[%d]", n_ldk))  # [4]
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.path <- function(x, ...) {
  color_fn <- get_pillar_color("path", "silver")

  formatted <- vapply(x, function(p) {
    if (is.na(p)) return(cli::col_grey("<NA>"))
    # Show just filename, not full path
    color_fn(basename(p))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.cur <- function(x, ...) {
  color_fn <- get_pillar_color("cur", "cyan")

  formatted <- vapply(x, function(coeffs) {
    if (!is.numeric(coeffs)) return(cli::col_grey("<NA>"))
    n_coeffs <- length(coeffs)
    color_fn(sprintf("(%d\u00B72)", n_coeffs))  # (80·2)
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.dct <- function(x, ...) {
  color_fn <- get_pillar_color("dct", "cyan")

  formatted <- vapply(x, function(coeffs) {
    if (!is.numeric(coeffs)) return(cli::col_grey("<NA>"))
    n_coeffs <- length(coeffs)
    color_fn(sprintf("%d dct", n_coeffs))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.eft <- function(x, ...) {
  color_fn <- get_pillar_color("eft", "cyan")

  formatted <- vapply(x, function(coeffs) {
    if (!is.matrix(coeffs)) return(cli::col_grey("<NA>"))
    n_harmonics <- nrow(coeffs)
    color_fn(sprintf("%d eft", n_harmonics))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.npoly <- function(x, ...) {
  color_fn <- get_pillar_color("npoly", "cyan")

  formatted <- vapply(x, function(coeffs) {
    if (!is.numeric(coeffs)) return(cli::col_grey("<NA>"))
    n_coeffs <- length(coeffs)
    color_fn(sprintf("%d np", n_coeffs))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.opoly <- function(x, ...) {
  color_fn <- get_pillar_color("opoly", "cyan")

  formatted <- vapply(x, function(coeffs) {
    if (!is.numeric(coeffs)) return(cli::col_grey("<NA>"))
    n_coeffs <- length(coeffs)
    color_fn(sprintf("%d op", n_coeffs))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}

#' @rdname pillar_methods
#' @export
pillar_shaft.proc <- function(x, ...) {
  color_fn <- get_pillar_color("proc", "green")

  formatted <- vapply(x, function(coords) {
    if (!is.matrix(coords)) return(cli::col_grey("<NA>"))
    n_ldk <- nrow(coords)
    color_fn(sprintf("%d proc", n_ldk))
  }, character(1))

  pillar::new_pillar_shaft_simple(formatted, align = "left")
}


# Color customization ----

#' Customize pillar colors
#'
#' Set custom colors for morphometric data types in tibble printing.
#'
#' @param ... Named arguments where names are class names and values are color names.
#'   Available colors: "blue", "red", "green", "yellow", "cyan", "magenta", "silver", "grey"
#'
#' @details
#' Colors can be set individually using options:
#' - `options(momocs2.pillar.color.out = "blue")`
#' - `options(momocs2.pillar.color.ldk = "red")`
#' - `options(momocs2.pillar.color.ldk_id = "red")`
#' - `options(momocs2.pillar.color.coo = "blue")`
#' - `options(momocs2.pillar.color.cur = "cyan")`
#' - `options(momocs2.pillar.color.dct = "cyan")`
#' - `options(momocs2.pillar.color.eft = "cyan")`
#' - `options(momocs2.pillar.color.npoly = "cyan")`
#' - `options(momocs2.pillar.color.opoly = "cyan")`
#' - `options(momocs2.pillar.color.proc = "green")`
#' - `options(momocs2.pillar.color.path = "silver")`
#'
#' Default colors:
#' - Coordinates: blue (out, coo)
#' - Landmarks: red (ldk, ldk_id)
#' - Coefficients: cyan (cur, dct, eft, npoly, opoly)
#' - Procrustes: green (proc)
#' - Paths: silver (path)
#'
#' @examples
#' \dontrun{
#' # Set all landmarks to magenta
#' options(momocs2.pillar.color.ldk = "magenta")
#' options(momocs2.pillar.color.ldk_id = "magenta")
#'
#' # Set outlines to green
#' options(momocs2.pillar.color.out = "green")
#'
#' # Reset to defaults (restart R or set NULL)
#' options(momocs2.pillar.color.out = NULL)
#' }
#'
#' @keywords internal
#' @name pillar_colors
NULL
