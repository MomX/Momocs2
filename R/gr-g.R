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

#' Plots all shapes on the shape graph
#'
#' @param x a coo_tbl object
#' @param ... additional parameters to feed ggplot2::geoms
#' @note former stack in old Momocs
#' @details `pile_column` is an helper function that takes a `coo_list` and
#' returns a single `tbl` with an additional name/index column.
#' @family family_picture
#' @examples
#'
#' bot2$coo %>% pile_column()
#' bot2 %>% pile()
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
pile.coo_tbl <- function(x, ...){
  x$coo %>% pile_column() %>%
    gg0() + ggplot2::aes(group=.data$shp) + ggplot2::geom_path()
}

#' @rdname pile
#' @export
pile_column <- function(x){
  x %>%
    purrr::imap(~.x %>% dplyr::mutate(shp=.y)) %>%
    # make it a single tbl
    dplyr::bind_rows() %>%
    # remove the class
    dplyr::as_tibble()
}



