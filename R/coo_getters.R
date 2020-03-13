# CENTROID ------------------------------------------------

# get_centpos --------------
#' Calculate centroid position
#'
#' Simply the average of `x` and `y`.
#'
#' @inheritParams coo_center
#' @return `numeric` or additional columns
#' @details This function can be used to integrate size - if meaningful -
#' @examples
#' bot2$coo[[5]] %>% get_centpos
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
get_centpos.list <- function(x) {
  x %>% purrr::map_df(get_centpos)
}

#' @export
get_centpos.coo_tbl <- function(x) {
  x$coo %>%
    purrr::map_df(get_centpos) %>%
    `colnames<-`(c("centpos_x", "centpos_y")) %>%
    dplyr::bind_cols(x, .)
}


# get_centsize --------------
#' Calculate centroid size
#'
#' Which is the square root of mean squared distances between each point
#' along the shape and its centroid coordinates.
#'
#' @inheritParams coo_center
#' @return `numeric` or additional column
#' @details Can be used, among others, to record size before [coo_scale].
#' @examples
#'
#' bot2$coo[[1]] %>% get_centsize
#'
#' @family coo_descriptors
#' @export
get_centsize <- function(x){
  UseMethod("get_centsize")
}

#' @export
get_centsize.default <- function(x) {
  df <- coo_single(x)
  sqrt(mean((df$x-mean(df$x))^2 + (df$y-mean(df$y))^2))
}

#' @export
get_centsize.list <- function(x){
  purrr::map_dbl(x, get_centsize)
}

#' @export
get_centsize.coo_tbl <- function(x){
  x %>% dplyr::mutate(centsize=purrr::map_dbl(x$coo, get_centsize))
}

# LW ------------------------------------------------------
# get_range -----------------------------------------------

#' Get shape range
#'
#' Just a wrapper around [range]
#'
#' @inheritParams coo_center
#'
#' @return `numeric` or additional column
#' @examples
#' bot2$coo[[5]] %>% get_range
#'
#' @family coo_descriptors
#' @export
get_range <- function(x){
  UseMethod("get_range")
}

#' @export
get_range.default <- function(x){
  x %>%
    coo_single() %>%
    dplyr::summarise(x_min=min(.data$x, na.rm=TRUE),
                     x_max=max(.data$x, na.rm=TRUE),
                     y_min=min(.data$y, na.rm=TRUE),
                     y_max=max(.data$y, na.rm=TRUE))
}

#' @export
get_range.list <- function(x){
  purrr::map_df(x, get_range)
}

#' @export
get_range.coo_tbl <- function(x){
  dplyr::bind_cols(x, purrr::map_df(x$coo, get_range))
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
#' bot2 %>% pick(1) %>% get_range
#' bot2 %>% get_diffrange()
#'
#' @family coo_descriptors
#' @export
get_range <- function(x){
  UseMethod("get_range")
}

#' @export
get_range.default <- function(x){
  x %>%
    coo_single() %>%
    dplyr::summarise(x_min=min(.data$x, na.rm=TRUE),
                     x_max=max(.data$x, na.rm=TRUE),
                     y_min=min(.data$y, na.rm=TRUE),
                     y_max=max(.data$y, na.rm=TRUE))
}

#' @export
get_range.list <- function(x){
  purrr::map_df(x, get_range)
}

#' @export
get_range.coo_tbl <- function(x){
  dplyr::bind_cols(x, purrr::map_df(x$coo, get_range))
}

# get_diffrange -------------------------------------------
#' @rdname get_range
#' @export
get_diffrange <- function(x){
  UseMethod("get_diffrange")
}

#' @export
get_diffrange.default <- function(x){
  x %>%
    coo_single() %>%
    dplyr::summarise(x_range=max(.data$x, na.rm=TRUE) - min(.data$x, na.rm=TRUE),
                     y_range=max(.data$y, na.rm=TRUE) - min(.data$y, na.rm=TRUE))
}

#' @export
get_diffrange.list <- function(x){
  purrr::map_df(x, get_diffrange)
}

#' @export
get_diffrange.coo_tbl <- function(x){
  dplyr::bind_cols(x, purrr::map_df(x$coo, get_diffrange))
}


# lw ------
#' Calculates length and width of a shape
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
#' bot2$coo[[5]] %>% get_lw
#' @export
get_lw <- function(x){
  UseMethod("get_lw")
}

#' @export
get_lw.default <- function(x) {
  x %>% coo_single() %>%
    coo_align() %>% get_range() %>%
    dplyr::transmute(x_range=abs(.data$x_max - .data$x_min),
                     y_range=abs(.data$y_max - .data$y_min))

}

#' @export
get_lw.list <- function(x){
  x %>% purrr::map_df(get_lw)
}

#' @export
get_lw.coo_tbl <- function(x){
  dplyr::bind_cols(x, get_lw(x$coo))
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
get_length.list <- function(x){
  purrr::map_dbl(x, get_length)
}

#' @export
get_length.coo_tbl <- function(x){
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
get_width.list <- function(x){
  purrr::map_dbl(x, get_width)
}

#' @export
get_width.coo_tbl <- function(x){
  x %>% dplyr::mutate(length=purrr::map_dbl(coo, get_width))
}
