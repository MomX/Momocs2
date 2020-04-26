# utils ---------------------------------------------------

# given a ggplot object, returns limits
.gg_lims <- function(x) {
  # build the plot and grab panel params
  w <- ggplot_build(x)$layout$panel_params[[1]]
  # nice formatting
  list(
    x_min = w$x.range[1],
    x_max = w$x.range[2],
    y_min = w$y.range[1],
    y_max = w$y.range[2]
  )
}

# given a ggplot object, returns range
.gg_range <- function(x) {
  # build the plot and grab panel params
  w <- ggplot_build(x)$layout$panel_params[[1]]
  # nice formatting
  list(
    x_range = w$x.range[2] - w$x.range[1],
    y_range = w$y.range[2] - w$y.range[1]
  )
}

# bind_distanciate_rows -----------------------------------
# handles grouping for ggplot2 graphics
.bind_distanciate_rows <- function(x, y){
  # throw an error if no group in x
  if (!("group" %in% names(x)))
    stop("bind_distanciate_rows: no 'group' column in x")
  m <- max(x$group)
  # if no group column, assumes it is unfolded
  # otherwise assume you know what you're doing
  if (!("group" %in% names(y)))
    y <- unfold(y)

  # if no grouping in y, create it and increment to avoid id crashes
  y <- dplyr::mutate(y, group=.data$group+m)
  # bind the two tibbles
  dplyr::bind_rows(x, y) %>% tibble::as_tibble()
}


# theme should come here ----------------------------------
# todo

# gg0 -----------------------------------------------------
# empty gg plots
# simply returns a gg from shape, but nothing drawn yet
#' @rdname gg
#' @export
gg0 <- function(x, ...){
  UseMethod("gg0")
}

#' @rdname gg
#' @export
gg0.default <- function(x, ...){
  .msg_info("gg0: no gg0 method for this class")
}

#' @rdname gg
#' @export
gg0.coo_single <- function(x, ...){
  x %>%
    unfold() %>%
    ggplot2::ggplot() +
    ggplot2::aes(x=.data$x, y=.data$y, group=.data$group) +
    ggplot2::coord_equal() +
    ggplot2::theme_void()
}

#' @rdname gg
#' @export
gg0.mom_tbl <- function(x, ...){
  x %>%
    # unfold() %>%
    ggplot2::ggplot() +
    ggplot2::aes(x=.data$x, y=.data$y, group=.data$group) +
    ggplot2::coord_equal() +
    ggplot2::theme_void()
}

# so that foreign tbl can be plotted too
# as long as they have 'x' and 'y' columns
# todo, ensure that
#' @rdname gg
#' @export
gg0.tbl <- gg0.coo_single


# gg ------------------------------------------------------
#' Universal graphics functions
#'
#' Default (ggplot2) visualisations for Momocs objects.
#'
#' @param x a Momocs object
#' @param first `logical` whether to draw first point
#' @param centroid `logical` whether to draw centroid
#' @param axes `logical` whether to draw axes, text and grid
#' @param gg `ggplot` object, default to [ggplot2::last_plot]
#' @param ... additional parameters to feed geoms
#' @note I call it "universal" as a reminder to provide a gg for each object.
#' @details `gg` is the base plotter.
#' `gg0` prepare the canvas but let you pick your `ggplot2::geoms`.
#' `draw` add shapes on top of last plot
#'
#' @return a `ggplot` object
#' @examples
#' b <- bot %>% pick(5)
#' gg(b)
#'
#' # Let's add some geoms to
#' nice_plot <- gg0(b) +
#'     ggplot2::geom_polygon(color="grey50", fill="red", alpha=0.25) +
#'     ggplot2::geom_point(shape="circle plus")
#' nice_plot # print it
#'
#' # you have all ggplot2 for free 8-)
#' # you do not have to ggplot2:: if you library(ggplot2) before
#' gorgeous_plot <- nice_plot + ggplot2::theme_minimal() +
#'     ggplot2::labs(x="abscissa", y="ordinate", title="Drink responsibly")
#'
#' # this is a plotting factory !
#' # gorgeous_plot %+% pick(bot, 12)
#' @rdname gg
#'
#' @export
gg <- function(x, first=TRUE, centroid=TRUE, axes=TRUE, ...) {
  UseMethod("gg")
}

#' @export
gg.default <- function(x, first=TRUE, centroid=TRUE, axes=TRUE, ...){
  .msg_info("gg: no gg method for this class")
}

#' @export
gg.coo_single <- function(x, first=TRUE, centroid=TRUE, axes=TRUE, ...){
  # prepare the canvas
  gg <- x %>% gg0()
  x <- unfold(x)
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

# gg methods ---------
# gg.pca <- function(x, f,
#                    x_axis=PC1, y_axis=PC2, ...){
#
#   # non standard evaluation
#   x_axis <- enquo(x_axis)
#   y_axis <- enquo(y_axis)
#
#
#   gg <- x %>% gg0(x_axis=!!x_axis, y_axis=!!y_axis, ...)
#
#   if (!missing(f)){
#     f <- enquo(f)
#     gg <- gg + ggplot2::aes(colour=!!f)
#   }
#
#   gg + geom_point()
#
# }

# gg0.pca <- function(x, x_axis=PC1, y_axis=PC2,
#                     morphospace_position=morphospace_grid_window,
#                     morphospace_size=NULL,
#                     morphospace_geom=ggplot2::geom_path, ...){
#
#   # non standard evaluation
#   x_axis <- enquo(x_axis)
#   y_axis <- enquo(y_axis)
#
#
#   # range
#   w <- x %>% dplyr::select(!!x_axis, !!y_axis) %>% abs %>% max()
#   w <- w*1.1
#
#   gg <- x %>%
#     ggplot2::ggplot() +
#     ggplot2::aes(x=!!x_axis, y=!!y_axis) +
#     ggplot2::coord_equal() +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
#     ggplot2::xlim(-w, w) + ggplot2::ylim(-w, w)
#   #
#   # # handles morphospcae
#   # if (!is.null(morphospace_position)){
#   #   xy <- x %>% dplyr::select(!!x_axis, !!y_axis) %>% morphospace_position(...)
#   #   morpho_tbl <- pca_to_coo(x, xy, k=morphospace_size) %>% unpack()
#   #   gg <- gg + morphospace_geom(data=morpho_tbl,
#   #                               mapping=ggplot2::aes(x=x, y=y, group=shp),
#   #                               inherit.aes=FALSE)
#   # }
#   gg$pca <- x
#   gg
# }
# Working place -----

# draw ----------------------------------------------------
#' Add shapes on top of another plot
#'
#'
#' @param x a Momocs object
#' @param gg `ggplot` object, default to [ggplot2::last_plot]
#' @param ... additional parameters to feed geoms
#'
#' @return a `ggplot` object
#' @export
#' @examples
#' bot %>% pick(1) %>% gg()
#' bot %>% pick(2) %>% draw()
#'
#' bot %>% pile(alpha=0.2)
#' bot %>% coo_rotate(pi/2) %>% draw(col="slateblue")
#' @export
draw <- function(x, gg, ...){
  UseMethod("draw")
}

#' @export
draw.default <- function(x, gg, ...){
  .msg_info("draw: no draw method for this class")
}

# geom_guesser here
# draw_outlines
# draw_polygon / # draw_outline
# draw_point / #draw_landmark
# draw_picture

#' @export
draw.coo_single <- function(x, gg=ggplot2::last_plot(), ...){
  gg %+% .bind_distanciate_rows(gg$data, unfold(x))
}

#' @export
draw.coo_list <- function(x, gg=ggplot2::last_plot(), ...){
  gg %+% .bind_distanciate_rows(gg$data, unfold(x))
}

#' @export
draw.mom_tbl <- function(x, gg=ggplot2::last_plot(), ...){
  gg %+% .bind_distanciate_rows(gg$data, unfold(x))
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
#' bot %>% inspect
#' }
#'
#' @export
inspect <- function(x, ...) {
  UseMethod("inspect")
}

#' @export
inspect.default <- function(x, ...){
  .msg_info("inspect: no method defined on this class")
}

#' @export
inspect.mom_tbl <- function(x, ...){
  repeat {
    readline(prompt = "Press <Enter> to continue, <Esc> to quit...")
    x %>% pick() %>% gg() %>% print()
  }
}


# decent --------------------------------------------------
#' Decent default values
#'
#' Aims at finding sesisble default values when not specified.
#'
#' Sizes are expressed in typographic points.
#' Mostly used internally, but can be overriden.
#' Will possibly be turned into options at some point.
#'
#' @param x `integer` a number on which to find a sensible value
#'
#' @examples
#' tibble::tibble(x=10^(1:5)) %>%
#'     dplyr::mutate_all(.funs=list(
#'         decimals   = decent_decimals,
#'         alpha      = decent_alpha,
#'         #geom       = decent_geom,
#'         size_path  = decent_size_path,
#'         size_point = decent_size_point,
#'         size_text  = decent_size_text
#'         ))
#' @name decent
NULL

#' @describeIn decent decimals
#' @export
decent_decimals  <- function(x)
  floor(log10(x))

#' @describeIn decent geom
#' @export
decent_geom      <- function(x)
  ifelse(x>60, ggplot2::geom_path, ggplot2::geom_point)

#' @describeIn decent alpha
#' @export
decent_alpha     <- function(x)
  c(1, 0.5, 0.25, 0.1, 0.05)[cut(x, breaks=c(-Inf, 10, 100, 1000, 10000, Inf))]

#' @describeIn decent size_geom
#' @export
decent_size <- function(x)
  c(1/.pt, 0.5/.pt, 0.25/.pt, 0.1/.pt, 0.05/.pt)[cut(x, breaks=c(-Inf, 10, 100, 1000, 10000, Inf))]

#' @describeIn decent size_path
#' @export
decent_size_path <- function(x)
  c(1/.pt, 0.5/.pt, 0.25/.pt, 0.1/.pt, 0.05/.pt)[cut(x, breaks=c(-Inf, 10, 100, 1000, 10000, Inf))]

#' @describeIn decent size_point
#' @export
decent_size_point <- function(x)
  c(1/.pt, 0.5/.pt, 0.25/.pt, 0.1/.pt, 0.05/.pt)[cut(x, breaks=c(-Inf, 10, 100, 1000, 10000, Inf))]


#' @describeIn decent size_text
#' @export
decent_size_text <- function(x)
  c(12/.pt, 10/.pt, 8/.pt, 6/.pt, 4/.pt)[cut(x, breaks=c(-Inf, 10, 100, 1000, 10000, Inf))]

# pile ----------------------------------------------------

#' Plot all shapes on the same graph
#'
#' @param x a mom_tbl object
#' @param ... additional parameters to pass to main geom
#' @param from_col column name to use for plotting (only for `mom_tbl` where it defaults to `coo`)
#' @param geom ggplot2 geom to use, typically `geom_path` or `geom_point`
#' @param size,alpha to feed geom
#'
#' @note formerly named `stack` in retired Momocs.
#' `stack` was quite a fine name but a `stack` is not something more fundamental in informatics.
#' Also, `pile` is shorter.
#' @family coo_graphics
#' @examples
#' bot %>% pick(1) %>% pile()
#' pile(bot$coo)
#' bot %>% dplyr::rename(coo2=coo) %>% pile(bot, from_col=coo2)
#' @name pile
NULL

#' @describeIn pile prepare
#' @export pile
pile0 <- function(x){
  ggplot2::ggplot(x) +
    ggplot2::aes(x=.data$x, y=.data$y, group=.data$group) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = element_blank(),
                   axis.title = element_blank(),
                   axis.text = element_text(size=8, colour = "grey40"),
                   axis.ticks = element_line(size = 0.2, colour = "grey40")) +
    scale_x_continuous(n.breaks = 2) +
    scale_y_continuous(n.breaks = 2)
}


#' @describeIn pile plotter
#' @export
pile <- function(x, ..., from_col, geom, size, alpha){
  UseMethod("pile")
}


# #' @describeIn pile plop_method
#' @export
pile.default <- function(x, ...){
  not_defined("pile")
}

# #' @describeIn pile coo_single_method
#' @export
pile.coo_single <- function(x, ..., from_col, geom, size=0.5, alpha=1){
  # find sensible values if missing
  if (missing(geom))  geom  <- decent_geom(nrow(x))

  # cook and return this beauty
  x %>% unfold %>% pile0() + geom(..., size=size, alpha=alpha)
}

#' @export
pile.coo_list <- function(x, ..., from_col, geom, size, alpha){
  # find decent values if not provided
  if (missing(geom))  geom  <- decent_geom(nrow(x[[1]]))
  # do we need sep for path and poitns here?
  if (missing(size))  size  <- decent_size_path(length(x))
  if (missing(alpha)) alpha <- decent_alpha(length(x))

  # cook and return this beauty
  x %>% unfold %>% pile0() + geom(..., size=size, alpha=alpha)

}

#' @export
pile.mom_tbl <- function(x, ..., from_col=coo, geom, size, alpha){

  # if missing, try to find sensible values
  n <- nrow(x)
  if (missing(geom))  geom  <- decent_geom(dplyr::pull(x, !!enquo(from_col))[[1]] %>% nrow())
  if (missing(size))  size  <- decent_size(n)
  if (missing(alpha)) alpha <- decent_alpha(n)

  # cook and return this beauty
  x %>% unfold({{from_col}}) %>% pile0() + geom(..., size=size, alpha=alpha)
}


# mosaic ----------------------------------------------

#' Plot all shapes as a mosaic
#'
#'
#' @param x a Momocs object
#' @param f a factor column to skip lines after the end of each level
#' @param ... additional parameters to feed `geom(...)`.
#' @param ncol `integer` that specifies the number of columns. Otherwise, a roughly squared mosaic is produced.
#' @param geom which `ggplot2::geom` to use ([ggplot2::geom_polygon] by default)
#' @return a `ggplot` object
#' @family family_picture
#' @examples
#' bot %>% mosaic()
#' bot %>% mosaic(type, ncol=6)
#' mosaic(bot, type, ncol=6, ggplot2::aes(col=type))
#'
#' @export
mosaic <- function(x, f, ..., ncol, geom) {
  UseMethod("mosaic")
}

# todo: go for mosaic0 and customs geoms / out, ldk, etc. ?
#' @export
mosaic.default <- function(x, ...) {
  .msg_info("mosaic: no mosaic method for this class")
}

#' @export
mosaic.mom_tbl <- function(x, f, ..., ncol, geom=geom_path) {

  # go for squarish defaults
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
      # ni: increment for each shape within each f level
      dplyr::mutate(ni=1:dplyr::n() - 1,
                    # deduces the column index: ci
                    ci=1 + ni %% ncol) %>%
      dplyr::ungroup() %>%
      # deduces the row index: ri
      dplyr::mutate(ri=cumsum(ifelse(ci==1, 1, 0))) -> df

    df <- mom(df) # todo vctrs
  }

  # invert ri, so that it wraps downwards
  df$ri <- max(df$ri) - df$ri + 1

  # drop useless columns and add proper classes
  df <- df %>%
    dplyr::select(-ni) %>%
    # template it with a small padding
    coo_template(size=0.95)

  df$coo <- purrr::pmap(list(df$coo, df$ci, df$ri),
                        ~coo_trans(..1, x_trans=..2, y_trans=..3)) %>% coo_list()

  df %>%
    gg0() + geom(...)
}

#
#
# x <- bot
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
# df <- df %>% .append_class("mom_tbl")
#
# df$coo <- purrr::pmap(list(coo_scale(coo_center(df$coo)), df$ci, df$ri),
#                       ~coo_trans(..1, x_trans=..2, y_trans=..3))
#



