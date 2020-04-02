# utils ---------------------------------------------------
# m is a matrix (eg of rotation). Must have colnames
# s is a named vector. Names must found among colnames(m)
# Prepare a diagonal matrix with provided elements of s
# In the end multiplies each column of m with matching element of s
.mprod <- function(m, s){
  .check(all(names(s) %in% colnames(m)),
         '.mprod: all "s" names must be present in "m"')
  # 0 numeric vector
  m2 <- vector("numeric", ncol(m))
  # named after m
  names(m2) <- colnames(m)
  # fill cols based on s names
  m2[names(s)] <- s
  # matrix multiplication
  res <- (m %*% diag(m2))
  # copy names
  colnames(res) <- colnames(m)
  res
}

# given a xy as returned by positionners retrieve sensible templating size
.sensible_templating_size <- function(x){
  template_minus_padding <- 0.95
  # get two consecutive elements so that it may work for circle too
  sqrt(sum((x[1, ] - x[2, ])^2))*template_minus_padding
}

# Given a number of partition
# returns a sensible arrangement
# for shape playground
# for n 1 -> 4 the arrangement is manual
# the produced table serves are addition coo_trans and coo_template
.morphospace_templating_table <- function(n){
  if (n==1){
    # early return for single partition
    return(tibble::tibble(number=1, x=0, y=0, size=1))
  }
  size <- ifelse(n<=4, 0.5, 1/(n-2))
  d <- 0.25 # to avoid repetitions
  if (n==2)
    res <- tibble::tibble(x=c(0, 0),
                          y=c(d, -d),
                          size=size)
  if (n==3)
    res <- tibble::tibble(x=c(0, -d, d),
                          y=c(d, -d, -d),
                          size=size)

  if (n==4)
    res <- tibble::tibble(x=c(-d, d, -d, d),
                          y=c(d, d, -d, -d),
                          size=size)

  if (n>4){
    # a reasonnable size (5=0.28, 6=0.22, 10=0.12, etc.)
    size <- 1/(n-2)
    # complex geometry will help here
    # first define a list of angles, starting at 12 o'clock
    # and in _anti_trigonemetric direction (ie clockwise)
    thetas <- seq(pi/2, 5*pi/2, length.out = n+1)[-(n+1)]  # because #1 is north
    res <- complex(modulus=1/3, argument=thetas) %>%
      complex_2_cartesian() %>%
      dplyr::mutate(size=size)
  }
  # add a partition column, reorder cols and return
  res %>%
    dplyr::mutate(number=1:dplyr::n()) %>%
    dplyr::select(.data$number, dplyr::everything())
}


# positionners --------------------------------
# rethink todo
morphospace_grid_window <- function(xy, nr=5, nc=6){
  # create sequence from range of each axis
  x <- seq(min(xy[, 1]), max(xy[, 1]), length.out = nr)
  y <- seq(min(xy[, 2]), max(xy[, 2]), length.out=nc)
  # return expanded grid
  tidyr::expand_grid(x, y) %>% `colnames<-`(colnames(xy))
}

morphospace_grid_abs <- function(xy, nr=5, nc=6){
  # get the extrema for each dimension
  xabs <- max(abs(xy[, 1]))
  yabs <- max(abs(xy[, 2]))
  # create sequence from range of each axis
  x <- seq(-xabs, xabs, length.out = nr)
  y <- seq(-yabs, yabs, length.out = nc)
  # return expanded grid
  tidyr::expand_grid(x, y) %>% `colnames<-`(colnames(xy))
}

morphospace_range <- function(xy){
  # get the extrema for each dimension
  xmin <- min(xy[, 1])
  xmax <- max(xy[, 1])
  ymin <- min(xy[, 2])
  ymax <- max(xy[, 2])
  # return a 4-row tibble
  tibble::tibble(x=c(xmin, xmax, 0,    0),
                 y=c(0,    0,    ymin, ymax)) %>%
    `colnames<-`(colnames(xy))
}

morphospace_circle <- function(xy, nb=12, r_centsize=1){
  # multiply the centsize radius by k
  r <- r_centsize*get_centsize(xy)
  # calculate regular positions
  t <- seq(0, 2 * pi, len = nb + 1)[-(nb + 1)]
  # return cartesian coordinates
  tibble::tibble(x=r * cos(t),
                 y=r * sin(t)) %>%
    `colnames<-`(colnames(xy))
}

# simply return positions
morphospace_xy <- function(xy){
  tibble::as_tibble(xy)
}

# morphospace ---------------------------------------------
#' Versatile function to reconstruct shapes from statistical objects
#'
#' @param x a statistical object. Currently supported: stat_pca
#' @param xy a tibble with positions to calculate. Default to morphospace_range on PC1 and PC2
#'
#' @return [coo_tbl] with raw shapes and other useful columns
#' @examples
#' x <- bot2 %>% efourier(6) %>% stat_pca()
#' pos1 <- tibble::tibble(PC1=seq(-5, 5, 1))
#' pos2 <- tibble::tibble(PC1=seq(-5, 5, 1), PC2=seq(-5, 5, 1))
#' m <- x %>% morphospace(pos1)
#' # both useless
#' m %>% pile()
#' m %>% mosaic()
#' m %>% morphospace_template() # warning is normal, only PC1 was passed
#' @export
morphospace <- function(x, xy){
  UseMethod("morphospace")
}

#' @export
morphospace.pca <- function(x, xy){

  if (missing(xy)){
    .msg_warning('morphospace_pca: "xy" not specified, using morphospace_range on PC1 and PC2 ')
    xy <- x %>% dplyr::select(.data$PC1, .data$PC2) %>% morphospace_range()
  }

  # # get_position coordinates
  # xy <- morphospace_fun(x, ...)
  rotation  <- attr(x, "rotation")
  mshape    <- attr(x, "mshape")

  # all partitions are calculated separetely
  # and .get_partition prepare a table with
  #  prefix and coe_list class
  partition <- .get_partition(x)

  # prepare a list for multiviews
  res <- vector("list", length=nrow(partition))
  names(res) <- partition$name

  # SHAPE CALCULATION
  for (i in 1:nrow(partition)){
    # subset based on prefixing
    partition1 <- partition[i, ]
    # turn the prefix into a slightly stronger regexpr
    prefix_regexpr <- paste0("^", partition1$name, "_")
    # which elements to retain for this partition
    retain <- stringr::str_detect(rownames(rotation), prefix_regexpr)
    # prepare these subsets
    rotation1  <- rotation[retain, ]
    mshape1    <- mshape[retain]

    # Now, loop over each coordinates from xy (morphospace positions)
    purrr::map(1:nrow(xy), ~{
      # pick concerned columns from rotation matrix
      ax_contrib <- rotation1 %>%
        # multiply values
        .mprod(unlist(xy[.x, ])) %>%
        # sum contribution of axes
        rowSums()
      # add it to mshape
      coe_predicted <- mshape1 + ax_contrib

      do.call(partition1$inverse, list(coe_predicted))
    }) %>% coo_list() %>% coo_tbl() -> z
    # purrr loop ends here

    # PREPARE FOR EXIT
    # repeat partition1 so that it fits with xy
    partition_long <- dplyr::slice(partition1, rep(1, nrow(xy)))
    xy <- xy #%>% dplyr::mutate_all(.funs = list(~.x %>% .append_class("axe_column"))) # very important for after
    # save into res
    res[[i]] <- dplyr::bind_cols(z, partition_long, xy)
  }

  # bind them all and return
  # suppressWarning since dplyr complaints for coo_tbl attributes
  # we call this beauty  "morpho_tbl"
  suppressWarnings(dplyr::bind_rows(res)) %>% coo_tbl() %>%
    dplyr::mutate(coo=coo_list(.data$coo))
}

# morphospace_template -------------------------------------
#' Template and adjust for multiviews before drawing morphospaces
#'
#' Digest information contained in the [coo_tbl] returned by [morphospace],
#' by adjusting for position (with [coo_trans]), overall templating ([coo_template]) and
#' multiviews adjustment (based on `x$name`).
#'
#' @param x [coo_tbl] returned by [morphospace]
#' @param size `numeric` or `ggplot` object.
#' To fine tune shape size or get a decent adjustment, respectively.
#' @return [coo_tbl] ready for ggplot.
#' @examples
#' p <- bot2 %>% efourier(6) %>% stat_pca()
#' m <- p %>% morphospace()
#' m %>% pile()
#' m %>% morphospace_template() %>% pile()
#'
#' gg <- p %>% gg0()
#' shps <- p %>% morphospace() %>% morphospace_template()
#' @export
morphospace_template <- function(x, size=NULL){

  # kinda weak but boy...
  xy <- x %>% dplyr::select(-(.data$coo:.data$inverse))

  # not a size directly provided
  if (missing(size) | is.null(size)){
    max_diff <- xy %>%
      purrr::map_dbl(~.x %>% range %>% diff) %>% max()
    max_number <- xy %>%
      purrr::map_dbl(~.x %>% unique %>% length) %>% max() %>% sqrt()
    size <- max_diff/max_number
  }

  # gg provided
  if (methods::is(size, "gg")){
    max_diff <- size %>% .gg_range() %>% unlist() %>% max()
    max_number <- xy %>%  purrr::map_dbl(~.x %>% unique %>% length) %>% max()
    size <- max_diff/max_number
  }

  x <- dplyr::bind_cols(
    x %>% dplyr::select(.data$coo, .data$name, .data$number),
    xy %>% `colnames<-`(c("x", "y")[1:ncol(xy)]))        # for the benefit of having cols named xy for after

  # 0.95 is general padding parameters to let some (5%) white space around shapes
  x <- x %>% dplyr::mutate(size=size*0.95)

  # handles single ax passed
  if (is.null(x$y))
    x <- x %>% dplyr::mutate(y=0)


  # handles partitions
  nb_partitions <- length(unique(x$name))
  templating_df <- .morphospace_templating_table(nb_partitions)

  # Now multiply/add size:positions
  res_df <- x %>%
    dplyr::left_join(templating_df, by="number", suffix=c("", "_adjust")) %>%
    dplyr::mutate(x=.data$x + (.data$x_adjust * .data$size),
                  y=.data$y + (.data$y_adjust * .data$size),
                  size=.data$size * .data$size_adjust)


  # todo rowwise
  # template to due size
  res_df$coo <- purrr::map2(res_df$coo, res_df$size, ~coo_template(.x, .y))

  # translate to where it belong
  vec_trans <- function(coo, x, y, ...) coo_trans(coo, x_trans=x, y_trans = y)
  res_df$coo <- purrr::pmap(res_df, vec_trans)

  # return this beauty
  res_df %>% dplyr::mutate(coo=coo_list(coo))
}

#' draw morphospace
#'
#' todo
#' @param x `ggplot` object
#' @param position foo
#' @param size foo
#' @param geom foo
#' @param ... foo
#' @return `ggplot` object
#'
#' @examples
#' #p <- bot2 %>% efourier(6) %>% stat_pca()
#' #p %>% gg0() %>% draw_morphospace() # adapt size
#' #p %>% morphospace() %>% morphospace_template()
#' @export
# draw_morphospace <- function(x, position=morphospace_grid_range(), size=x, geom=ggplot2::geom_path, ...){
#   gg <- x       # the x is actually a gg here
#   pca <- gg$pca # grab the pca component useful downwards
#
#   # use gg$mapping to retrieve concerned columns
#   xy <- dplyr::select(pca, !!gg$mapping[[1]], !!gg$mapping[[2]]) %>%
#     position()
#
#   # # if missing size, pass gg to morphospace_template
#   # if (missing(size)) size <- gg
#
#   shp_df <- pca %>%
#     morphospace(xy = xy) %>%
#     morphospace_template(size = size) %>% unfold()
#
#   gg + geom(data=shp_df,
#             mapping=ggplot2::aes(x=.data$x, y=.data$y, group=.data$shp),
#             col="grey20",
#             inherit.aes=FALSE, ...)
# }



