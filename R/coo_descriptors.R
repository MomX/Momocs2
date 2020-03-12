# get_centpos --------------
#' Calculate centroid position
#'
#' Simply the average of `x` and `y`.
#' @inheritParams coo_center
#' @return `numeric` or additional columns
#' @details This function can be used to integrate size - if meaningful -
#' @examples
#'
#' bot2$coo[[1]] %>% get_centpos
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
  x %>% purrr::map(get_centpos)
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
  purrr::map(x, get_centsize)
}

#' @export
get_centsize.coo_tbl <- function(x){
  x %>% dplyr::mutate(centsize=purrr::map_dbl(x$coo, get_centsize))
}

