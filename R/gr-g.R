#' ggplot2 plotter
#'
#' Prepare and build ggplot2 graphics from common classes
#'
#' @param x object with a defined method
#' @param ... additional parameters
#'
#' @export
g <- function(x, ...){
  UseMethod("g")
}


#' @export
g.coo_single <- function(x, ...){
  x %>%
    ggplot() +
    aes(x=.data$x, y=.data$y) +
    coord_equal() +
    # geom_hline(yintercept=0, linetype="twodash", colour="grey40", size=1/.pt) +
    # geom_vline(xintercept=0, linetype="twodash", colour="grey40", size=1/.pt) +
    geom_point(data=dplyr::summarize_all(x, mean), shape="plus") +
    # geom_point(data=tibble::tibble(x=0, y=0), shape="circle open") +

    geom_point(data=dplyr::slice(x, 1), shape="triangle filled") +

    # restore arrow
    # geom_segment(mapping=aes(x=.data$x0, xend=.data$x1, y=.data$y0, yend=.data$y1),
    #              data=tibble::tibble(x0=x[[1, 1]], x1=x[[2, 1]],
    #                                  y0=x[[2, 1]], y1=x[[2, 2]]),
    #              arrow=arrow(length = unit(1/40, "npc"))) +
    # scale_x_continuous(breaks = scales::breaks_pretty(n = 5), limits=range(x$x)) +
    # scale_y_continuous(breaks = scales::breaks_pretty(n = 5), limits=range(x$y)) +
    geom_path() +
    labs(x="", y="") +
    theme_minimal() +
    theme(panel.grid.minor  = element_blank())
}
