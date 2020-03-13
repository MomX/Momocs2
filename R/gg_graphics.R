#' ggplot2 default plotter
#'
#' Default ((gplot2) visualisations for Momocs objects.
#'
#' @param x a Momocs object
#' @param first `logical` whether to draw first point
#' @param centroid `logical` whether to draw centroid
#' @param axes `logical` whether to draw axes, text and grid
#' @param ... additional parameters to feed geoms
#'
#' @details `gg0` prepare the canvas but let you pick your `ggplot2::geoms`.
#'
#' @return a `ggplot` object
#' @rdname gg
#' @export
#' @examples
#' bot2 %>% pick(1) %>% gg()
#' bot2 %>% pick(1) %>% gg0() + ggplot2::geom_point(shape="circle plus")
#'


# gg0 -----------------------------------------------------
# empty gg plots
# simply returns a gg from shape, but nothing drawn yet
#' @export
gg0 <- function(x, ...){
  UseMethod("gg0")
}

#' @export
gg0.default <- function(x, ...){
  .msg_info("no gg0 method for this class")
}

#' @export
gg0.coo_single <- function(x, ...){
  x %>%
    ggplot2::ggplot() +
    ggplot2::aes(x=.data$x, y=.data$y) +
    ggplot2::coord_equal() +
    ggplot2::theme_void()
}


#' @export
gg0.tbl <- gg0.coo_single

# gg ------------
# basic plot for gg
#' @rdname gg
#' @export
gg <- function(x, ...) {
  UseMethod("gg")
}

#' @rdname gg
#' @export
gg.default <- function(x, ...){
  .msg_info("no gg method for this class")
}

#' @rdname gg
#' @export
gg.coo_single <- function(x, first=TRUE, centroid=TRUE, axes=TRUE, ...){
  # prepare the canvas
  gg <- gg0(x)

  # add more on demand
  #   first point
  if (first)
    gg <- gg + ggplot2::geom_point(data=dplyr::slice(x, 1), shape="triangle filled")
  #   centroid
  if (centroid)
    gg <- gg + ggplot2::geom_point(data=dplyr::summarize_all(x, mean), shape="plus")
  if (axes)
    gg <- gg + ggplot2::theme_minimal()

  # tries to guess
  n <- nrow(x)
  if (n < 60)
    gg <- gg + ggplot2::geom_point(..., shape="plus") # option here
  else
    gg <- gg + ggplot2::geom_path(...)
  # return this beauty
  gg
}

# inspect -------------------------------------------------

#' Graphical inspection of shapes
#'
#' Sample one shape, plot it with [gg], repeat.
#'
#' @param x a Momocs object
#' @param ... additional parameters forwarded to [gg]
#' @family family_picture
#' @examples
#' \dontrun{
#' bot2 %>% inspect
#' }
#'
#' @export
inspect <- function(x, ...) {
  UseMethod("inspect")
}

#' @export
inspect.default <- function(x, ...){
  .msg_info("no inspect method for this class")
}

#' @export
inspect.coo_tbl <- function(x, ...){
  repeat {
    readline(prompt = "Press <Enter> to continue, <Esc> to quit...")
    x %>% pick() %>% gg() %>% print()
  }
}

# pile ----------------------------------------------------

#' Plot all shapes on the same graph
#'
#' @param x a coo_tbl object
#' @param ... additional parameters to feed ggplot2::geoms
#' @note former stack in old Momocs
#' @details `pile_column` is an helper function that takes a `coo_list` and
#' returns a single `tbl` with an additional name/index column.
#' @family family_picture
#' @examples
#' bot2 %>% pile()
#' # global spec
#' bot2 %>% pile(col="gold")
#' # aes bounded to x columns
#' bot2 %>% pile(type) # you can omit ggplot2:: if it's loaded
#'
#' # a more complex example
#' bot2 %>% pile(type) +
#'    ggplot2::facet_grid(~type) +
#'    ggplot2::scale_colour_manual(values=c("forestgreen", "orange"))
#' @export
pile <- function(x, ...) {
  UseMethod("pile")
}

#' @export
pile.default <- function(x, ...){
  .msg_info("no pile method for this class")
}

#todo: decent_alpha
# manage pile.coo_out
#' @export
pile.coo_tbl <- function(x, f, ...){
  gg <- x %>% unpack() %>% gg0() + aes(group=.data$shp) # perhaps group wont be used for ldks
  if (missing(f)){
    gg <- gg + ggplot2::geom_path(...)
  } else {
    f <- enquo(f)
    gg <- gg + aes(colour=!!f) + ggplot2::geom_path(...)
  }
  # return this beauty
  gg
}

# mosaic ----------------------------------------------

#' Plot all shapes as a mosaic
#'
#'
#' @param x a Momocs object
#' @param f a factor column to skip lines after the end of each level
#' @param ... additional parameters to feed `geom(...)`.
#' @param geom which `ggplot2::geom` to use ([ggplot2::geom_polygon] by default)
#' @param ncol `integer` that specifies the number of columns. Otherwise, a roughly squared mosaic is produced.
#' @return a `ggplot` object
#' @family family_picture
#' @examples
#' bot2 %>% mosaic()
#' bot2 %>% mosaic(type, ncol=6)
#' mosaic(bot2, type, ncol=6, aes(col=type))
#'
#' @export
mosaic <- function(x, ...) {
  UseMethod("mosaic")
}

#' @export
mosaic.default <- function(x, ...) {
  .msg_info("no mosaic method for this class")
}

#' @export
mosaic.coo_tbl <- function(x, f, ..., ncol, geom=geom_path) {
  if (missing(ncol)){
    ncol <- ceiling(sqrt(nrow(x)))
  }

  # prepare a tbl that will define translation values
  # for templated shapes
  # ri= row index (x_trans); ci=column index (y_trans)
  if (missing(f)){
    x %>%
      dplyr::mutate(ni=1:dplyr::n() - 1,
                    ci=1 + ni %% ncol) %>%
      dplyr::mutate(ri=cumsum(ifelse(ci==1, 1, 0))) -> df
  } else {
    # if f is provided, skip a line when level ends
    f <- enquo(f)
    x %>%
      dplyr::group_by(!!f) %>%
      # ni increment for each shape within each f level
      dplyr::mutate(ni=1:dplyr::n() - 1,
                    # deduces the column index
                    ci=1 + ni %% ncol) %>%
      dplyr::ungroup() %>%
      # deduces the row index
      dplyr::mutate(ri=cumsum(ifelse(ci==1, 1, 0))) -> df
  }

  # invert ri, so that it wraps downwards
  df$ri <- max(df$ri) - df$ri + 1
  #
  # drop useless columns and add proper classes
  df <- df %>%
    dplyr::select(-ni) %>%
    dplyr::mutate(coo=.append_class(coo, "coo_list")) %>%
    .append_class("coo_tbl") %>%
    # template it with a small padding
    coo_template(size=0.95)

  df$coo <- purrr::pmap(list(df$coo, df$ci, df$ri),
                        ~coo_trans(..1, x_trans=..2, y_trans=..3))

  df %>% unpack() %>%
    gg0() + ggplot2::aes(group=shp) + geom(...)
}

#
#
# x <- bot2
# ns <-
#   nc <- ceiling(sqrt(nrow(x)))
#
# x %>%
#   dplyr::group_by(fake) %>%
#   dplyr::mutate(ni=1:dplyr::n() - 1,
#                 ci=1 + ni %% nc) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(ri=cumsum(ifelse(ci==1, 1, 0))) %>%
#   dplyr::select(-ni) -> df
#
# df$coo <- df$coo %>% .append_class("coo_list")
# df <- df %>% .append_class("coo_tbl")
#
# df$coo <- purrr::pmap(list(coo_scale(coo_center(df$coo)), df$ci, df$ri),
#                       ~coo_trans(..1, x_trans=..2, y_trans=..3))
#
# df %>% unpack()
#
# df %>% unpack() %>%
#   gg0() + aes(group=shp) + geom_polygon(aes(col=type, fill=fake), alpha=0.5)
#


