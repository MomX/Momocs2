# CENTROID AND CO -----------------------------------------

# get_centpos --------------
#' Calculate centroid position
#'
#' Simply the average of `x` and `y`.
#'
#' @inheritParams coo_center
#' @return `numeric`, list or additional columns
#' @details This function can be used to integrate size - if meaningful -
#' @examples
#' bot$coo[[5]] %>% get_centpos
#'
#' @family coo_descriptors
#' @export
get_centpos <- function(x) {
  UseMethod("get_centpos")
}

#' @export
get_centpos.default <- function(x) {
  x %>% coo_single() %>% dplyr::summarise_all(mean)
}

#' @export
get_centpos.coo_list <- function(x) {
  x %>%
    purrr::map_df(get_centpos) %>%
    tibble::as_tibble() %>%
    `colnames<-`(c("centpos_x", "centpos_y"))
}

#' @export
get_centpos.mom_tbl <- function(x) {
  dplyr::bind_cols(tibble::as_tibble(x),
                   get_centpos(x$coo)) %>%
    new_mom()
}


# get_centsize --------------
#' Calculate centroid size
#'
#' [get_centsize] provides the classical centroid size which is the
#' square root of the _sum_ of squared distances between each point
#' along the shape and its centroid coordinates. [get_centsize_norm] is the
#' square root of the _mean_ of squared distances between each point
#' along the shape and its centroid coordinates. See Details.
#'
#' [get_centsize_norm] does not depend (so much) on the number of points of the shape (see Examples),
#' makes direct sense as a dimension in the plane, and
#' also when the number of points differs between shapes, which is both typical
#' for outlines and never seen for landmarks.
#' See Dryden I. E. and Mardia K. V. 1998. Statistical Shape Analysis. Wiley,
#'  Chichester. Since [get_centsize] never made a lot of sense to me, as an "outline" person, and because
#'  [get_centsize_norm] will "work" in all cases, it is the default method in [coo_scale]
#'
#' @inheritParams coo_center
#' @return `numeric` or additional column
#' @details Can be used, among others, to record size before [coo_scale].
#' @examples
#'
#' b <- bot %>% pick(1)
#' b %>% get_centsize
#' b %>% coo_sample(64) %>% get_centsize()
#'
#' b %>% get_centsize_norm()
#' b %>% coo_sample(64) %>% get_centsize_norm()
#' @family coo_descriptors
#' @export
get_centsize <- function(x){
  UseMethod("get_centsize")
}

#' @export
get_centsize.default <- function(x){
  not_defined("get_centsize")
}

#' @export
get_centsize.coo_single <- function(x) {
  sqrt(sum((x$x-mean(x$x))^2 + (x$y-mean(x$y))^2))
}

#' @export
get_centsize.coo_list <- function(x){
  purrr::map_dbl(x, get_centsize) %>% tibble::tibble(centsize=.)
}

#' @export
get_centsize.mom_tbl <- function(x){
  x %>% dplyr::mutate(centsize=purrr::map_dbl(x$coo, get_centsize))
}


#' @describeIn get_centsize normalized version
#' @export
get_centsize_norm <- function(x){
  UseMethod("get_centsize_norm")
}

#' @export
get_centsize_norm.default <- function(x){
  not_defined("get_centsize_norm")
}

#' @export
get_centsize_norm.default <- function(x) {
  sqrt(mean((x$x-mean(x$x))^2 + (x$y-mean(x$y))^2))
}

#' @export
get_centsize_norm.coo_list <- function(x){
  purrr::map_dbl(x, get_centsize_norm) %>% tibble::tibble(centsize=.)
}

#' @export
get_centsize_norm.mom_tbl <- function(x){
  x %>% dplyr::mutate(centsize=purrr::map_dbl(x$coo, get_centsize_norm))
}


# PERIM AND CO --------------------------------------------

# get_perimpts_along --------------
#' Calculate perimeter and variations
#'
#' @description
#' * `get_perim_along` calculate the euclidean distance between every points of a shape
#' * `get_perim` is simply `sum(coo_perim_along)`
#' * `get_perim_cumsum` is simply `cumsum(coo_perim_along)`
#'
#' @inherit get_centsize params return
#' @family getters
#' @family perimeter getters
#' @examples
#' bot %>% pick(1) %>% get_perim_along()
#' @export
get_perim_along <- function(x) {
  UseMethod("get_perim_along")
}

#' @export
get_perim_along.coo_single <- function(x){
  x %>%
    # create two 1lagged columns on which euclidean distance will be calculated
    # default ensure that last distance is d(last-first)
    dplyr::mutate(x2=dplyr::lag(.data$x, 1, default=.data$x[1]),
                  y2=dplyr::lag(.data$y, 1, default=.data$y[1])) %>%
    dplyr::transmute(d=sqrt((.data$x - .data$x2)^2 + (.data$y - .data$y2)^2)) %>%
    tibble::as_tibble() # drops mom_tbl
}

#' @export
get_perim_along.coo_list <- function(x){
  x %>% purrr::map(get_perim_along)
}

#' @export
get_perim_along.mom_tbl <- function(x){
  x %>%
    dplyr::mutate(perim_along=purrr::map(x$coo, get_perim_along))
}

# get_perim --------
#' @describeIn get_perim_along Calculate total perimeter
#' @export
get_perim <- function(x){
  UseMethod("get_perim")
}

#' @export
get_perim.coo_single <- function(x){
  x %>% get_perim_along() %>% sum()
}

#' @export
get_perim.coo_list <- function(x){
  x %>% purrr::map_dbl(get_perim) %>% tibble::as_tibble()
}

#' @export
get_perim.mom_tbl <- function(x){
  x %>% dplyr::mutate(perim=purrr::map_dbl(x$coo, get_perim))
}

# get_perim_cumsum --------
#' @describeIn get_perim_along Calculate cumsum between successive points of a shape
#' @export
get_perim_cumsum <- function(x){
  UseMethod("get_perim_cumsum")
}

#' @export
get_perim_cumsum.coo_single <- function(x){
  x %>% get_perim_along() %>% cumsum()
}

#' @export
get_perim_cumsum.coo_list <- function(x){
  x %>% purrr::map(get_perim_cumsum) %>% tibble::tibble(perim_cumsum=.)
}

#' @export
get_perim_cumsum.mom_tbl <- function(x){
  dplyr::bind_cols(tibble::as_tibble(x),
                   get_perim_cumsum(x$coo)) %>%
    new_mom()
}

# LENGTH, WIDTH AND CO ------------------------------------
# get_range -----------------------------------------------

#' Get shape range
#'
#' Just a wrapper around [range]
#'
#' @inheritParams coo_center
#'
#' @return `numeric` or additional column
#' @examples
#' bot$coo[[5]] %>% get_range
#'
#' @family coo_descriptors
#' @export
get_range <- function(x){
  UseMethod("get_range")
}

#' @export
get_range.coo_single <- function(x){
  x %>%
    dplyr::summarise(x_min=min(.data$x, na.rm=TRUE),
                     x_max=max(.data$x, na.rm=TRUE),
                     y_min=min(.data$y, na.rm=TRUE),
                     y_max=max(.data$y, na.rm=TRUE)) %>%
    tibble::as_tibble()
}

#' @export
get_range.coo_list <- function(x){
  purrr::map_df(x, get_range)
}

#' @export
get_range.mom_tbl <- function(x){
  dplyr::bind_cols(tibble::as_tibble(x),
                   get_range(x$coo)) %>%
    new_mom()
}

# get_diffrange -----------------------------------------------

#' Get shape range
#'
#' Just a wrapper around [range]. `get_diffrange` adds [diff] to `get_range`.
#'
#' @inheritParams coo_center
#'
#' @return `numeric` or additional columns
#' @examples
#' bot %>% pick(1) %>% get_range
#' bot %>% get_diffrange()
#'
#' @family coo_descriptors
#' @export
get_range <- function(x){
  UseMethod("get_range")
}

#' @export
get_range.coo_list <- function(x){
  x %>%
    dplyr::summarise(x_min=min(.data$x, na.rm=TRUE),
                     x_max=max(.data$x, na.rm=TRUE),
                     y_min=min(.data$y, na.rm=TRUE),
                     y_max=max(.data$y, na.rm=TRUE))
}

#' @export
get_range.coo_list <- function(x){
  purrr::map_df(x, get_range)
}

#' @export
get_range.mom_tbl <- function(x){
  dplyr::bind_cols(tibble::as_tibble(x),
                   get_range(x$coo)) %>%
    new_mom()
}

# get_diffrange -------------------------------------------
#' @rdname get_range
#' @export
get_diffrange <- function(x){
  UseMethod("get_diffrange")
}

#' @export
get_diffrange.coo_single <- function(x){
  x %>%
    dplyr::summarise(x_range=max(.data$x, na.rm=TRUE) - min(.data$x, na.rm=TRUE),
                     y_range=max(.data$y, na.rm=TRUE) - min(.data$y, na.rm=TRUE))
}

#' @export
get_diffrange.coo_list <- function(x){
  purrr::map_df(x, get_diffrange) %>% tibble::as_tibble()
}

#' @export
get_diffrange.mom_tbl <- function(x){
  dplyr::bind_cols(tibble::as_tibble(x),
                   get_diffrange(x$coo)) %>%
    new_mom()
}


# lw ------
#' Calculate length and width of a shape
#'
#' Returns the length and width of a shape. The shape is first aligned using
#' [coo_align], then length is the range along the x-axis and the width as
#' the range on the y-axis.
#' @inheritParams coo_center
#' @return `numeric` or additional column
#'
#' @rdname get_lw
#' @family coo_ descriptors
#' @examples
#' bot$coo[[5]] %>% get_lw
#' @export
get_lw <- function(x){
  UseMethod("get_lw")
}

#' @export
get_lw.coo_single <- function(x) {
  x %>%
    coo_align() %>% get_range() %>%
    dplyr::transmute(x_range=abs(.data$x_max - .data$x_min),
                     y_range=abs(.data$y_max - .data$y_min))

}

#' @export
get_lw.coo_list <- function(x){
  x %>% purrr::map_df(get_lw) %>% tibble::as_tibble()
}

#' @export
get_lw.mom_tbl <- function(x){
  dplyr::bind_cols(tibble::as_tibble(x),
                   get_lw(x$coo)) %>%
    new_mom()
}

# length --------------------------------------------------
#' @rdname get_lw
#' @export
get_length <- function(x){
  UseMethod("get_length")
}

#' @export
get_length.default <- function(x) {
  get_lw(x)$x_range
}

#' @export
get_length.coo_list <- function(x){
  purrr::map_dbl(x, get_length)
}

#' @export
get_length.mom_tbl <- function(x){
  x %>% dplyr::mutate(length=purrr::map_dbl(coo, get_length))
}

# get_width -----------------------------------------------
#' @rdname get_lw
#' @export
get_width <- function(x){
  UseMethod("get_width")
}

#' @export
get_width.default <- function(x) {
  get_lw(x)$y_range
}

#' @export
get_width.coo_list <- function(x){
  purrr::map_dbl(x, get_width)
}

#' @export
get_width.mom_tbl <- function(x){
  x %>% dplyr::mutate(width=purrr::map_dbl(coo, get_width))
}

# get_nb --------------------------------------------------
#' Calculate the number of coordinates per shape
#'
#' Wraps around `nrow`
#'
#' @inherit get_centsize params return
#' @family getters
#' @family shape getters
#' @examples
#' bot %>% pick(1) %>% get_nb()
#' bot %>% get_nb()
#' @export
get_nb <- function(x) {
  UseMethod("get_nb")
}

#' @export
get_nb.coo_single <- function(x){
  nrow(x)
}

#' @export
get_nb.coo_list <- function(x){
  purrr::map_dbl(x, nrow)
}

#' @export
get_nb.mom_tbl <- function(x){
  x %>% dplyr::mutate(nb=purrr::map_dbl(coo, nrow))
}


