
# utils ---------------------------------------------------

# Given a PCA returns the partition table
.get_partition <- function(x){
  # retrieve meanshape and partition classes
  mshape <- attr(x, "mshape")
  classes_df <- attr(x, "coe_class") %>% tibble::enframe()

  # turn mshape names into tibble
  mshape %>%
    tibble::enframe() %>%
    # only retain prefix
    dplyr::transmute(name=.data$name %>% stringr::str_remove("_.*$"),
                     # add a numeric column (not sure it will help)
                     number=.data$name %>% factor() %>% as.numeric()) %>%
    # join with methods
    dplyr::left_join(classes_df, by="name") %>%
    # prepare inverse
    dplyr::mutate(inverse=paste0(.data$value, "_i")) %>%
    # only distinct rows
    dplyr::distinct()
}

# stat_pca ------------------------------------------------
#' Principal component analysis
#'
#' Performs a principal components analysis (delegated to [stats::prcomp]).
#'
#' @param x [coe_tbl]
#' @param center `logical` whether the variables should be shifted to
#' be zero centered (`center` in [stats::prcomp])
#' @param scale `logical` whether the variables should be scaled to
#' have unit variance before the analysis takes place (`scale.` in [stats::prcomp])
#'
#' @return object of class `pca`, a [tibble::tibble] with attributes
#' @references [stats::prcomp]
#'
#' @examples
#' (x <-  bot2 %>% efourier(4) %>% stat_pca())
#' attributes(x)
#' @export
stat_pca <- function(x, center=TRUE, scale=TRUE){
  if (missing(center))
    .msg_warning("describe_pca: 'center' was missing, set to TRUE")
  if (missing(scale))
    .msg_warning("describe_pca: 'scale' was missing, set to TRUE")

  # get coe columns and send them to prcomp
  x %>%
    dplyr::select_if(is_coe_list) %>%
    # rename cols within coe_list
    prefix_col() %>%
    # bind_rows all coe_lists
    purrr::map(dplyr::bind_rows) %>%
    # unite them
    dplyr::bind_cols() -> coeffs

  # delegate pca to prcomp
  pca    <- stats::prcomp(coeffs, center=center, scale.=scale)

  # join back scores
  res <- dplyr::bind_cols(
    x,
    dplyr::as_tibble(pca$x)
  )

  # save meta as attributes
  attr(res, "method")    <- "prcomp"
  attr(res, "coe_class") <- x %>% dplyr::select_if(is_coe_list) %>% purrr::map_chr(~class(.x)[1])
  attr(res, "center")    <- pca$rotation
  attr(res, "scale")     <- pca$rotation
  attr(res, "rotation")  <- pca$rotation
  attr(res, "sdev")      <- pca$sdev
  attr(res, "mshape")    <- apply(coeffs, 2, mean) # NA handling required?

  # decorate and return this beauty
  res %>% .append_class("pca")
}

# print ---
#' @export
print.pca <- function(x, ...){
  x %>% tibble::as_tibble() %>% print(...)  # to use base tbl print method
  glue::glue(cli::symbol$pointer, " a ",
             crayon::bgGreen("stat_pca"), " ",
             "with {ncol(x)} shapes") %>%
    cli::cat_line()
  # just as print.default
  invisible(x)
}


# scree ---------------------------------------------------
#' How many axes to retain this much of variance or trace ?
#'
#' A set of functions around PCA/LDA eigen/trace.
#'
#' * `scree` calculates their proportion and cumulated proportion;
#' * `scree_min` returns the minimal number of axis to use to retain a given proportion;
#' * `scree_plot` displays a screeplot.
#'
#' @param x a stat object (`scree`) or a tibble returned by `scree` for `scree_*`
#' @param n_axes `numeric` range of axes to consider.
#' All by default for `scree_min`, display until `0.99` for `scree_plot`
#' @param prop numeric how many axes are enough to gather this proportion of variance.
#' Default to 1, all axes are returned
#' defaut to 1: all axis are returned
#' @return scree returns a data.frame, scree_min a numeric, scree_plot a ggplot.
#' @examples
#' p <- bot2 %>% efourier(6) %>% stat_pca()
#' p %>% scree()
#' p %>% scree_min(0.99)
#' p %>% scree_plot(prop=0.95)
#' @export
#' @rdname scree
scree <- function(x, n_axes) {
  UseMethod("scree")
}

#' @export
#' @rdname scree
scree.default <- function(x, n_axes) {
  .msg_info("scree: no method defined for this class")
}


#' @export
#' @rdname scree
scree.pca <- function(x, n_axes){
  # calculate proportion for each axi
  sdev <- attr(x, "sdev")
  eig <- (sdev^2)
  eig <- eig / sum(eig)

  # if n_axes not provided, take all
  if (missing(n_axes))
    n_axes <- length(eig)

  axes_range <- 1:n_axes
  axes_names <- attr(x, "rotation") %>% colnames()

  # return a data_frame
  tibble::tibble(axis       = axes_names[axes_range],
                 number     = seq_along(axes_names)[axes_range],
                 proportion = eig[axes_range],
                 cumsum     = cumsum(eig)[axes_range])
}

# scree_min -------
#' @describeIn scree minimal number of axes to retain
#' @export
scree_min <- function(x, prop){
  UseMethod("scree_min")
}

#' @export
scree_min.default <- function(x, prop){
  .msg_warning("scree_min: no method defined for this class")
}

#' @export
scree_min.pca <- function(x, prop){
  if (missing(prop)){
    .msg_info('scree_min: "prop" was missing and set to 0.99')
    prop <- 0.99
  }
  x %>% scree() %>%
    dplyr::filter(.data$cumsum<=prop) %>%
    dplyr::top_n(1, .data$number) %>%
    dplyr::pull(.data$number)
}

# scree_plot ------
# scree_plot
#' @describeIn scree minimal number of axes to retain
#' @export
scree_plot<- function(x, n_axes, prop){
  UseMethod("scree_plot")
}

#' @export
scree_plot.default <- function(x, n_axes, prop){
  .msg_warning("scree_plot: no method defined for this class")
}

#' @export
scree_plot.pca <- function(x, n_axes, prop){
  if (missing(n_axes)){
    n_axes <- scree_min(x, prop=prop)
  }

  x %>% scree() %>%
    dplyr::slice(1:n_axes) %>%
    dplyr::mutate(cumsum=signif(.data$cumsum, 3)) %>%
    ggplot2::ggplot() +
    ggplot2::aes(x=.data$axis, y=.data$proportion, label=.data$cumsum) +
    ggplot2::geom_col() +
    ggplot2::geom_text(vjust=-0.1) +
    ggplot2::labs(x="number of axes",
                  y="proportion of variance") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid =ggplot2::element_blank())
}

#' @rdname gg
#' @param x_axis column name
#' @param y_axis column name
#' @export
gg0.pca <- function(x, x_axis, y_axis, ...){
  # tidyeval + handle missing args
  if (missing(x_axis))
    x_axis <- quo(PC1)
  else
    x_axis <- enquo(x_axis)

  if (missing(y_axis))
    y_axis <- quo(PC2)
  else
    y_axis <- enquo(y_axis)

  # define lims so that it's centered
  w <- x %>%
    dplyr::select(!!x_axis, !!y_axis) %>%
    abs() %>% max()

  # add var to PC names
  s <- x %>% scree()
  x_var <- dplyr::filter(s, .data$axis==quo_name(x_axis))$proportion
  xlab <- paste0(quo_name(x_axis), " (", round(100*x_var, 1), "%)")
  y_var <- dplyr::filter(s, .data$axis==quo_name(y_axis))$proportion
  ylab <- paste0(quo_name(y_axis), " (", round(100*y_var, 1), "%)")


  ggplot2::ggplot(x) +
    ggplot2::aes(x=!!x_axis, y=!!y_axis) +
    ggplot2::coord_equal(xlim = c(-w, w), ylim=c(-w, w)) +
    ggplot2::labs(x=xlab, y=ylab) +
    theme_minimal()
}



