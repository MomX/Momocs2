# coo_centpos --------------
#' Calculate centroid position
#'
#' Simply the average of `x` and `y`.
#' @inheritParams coo_center
#' @return `numeric` or additional columns
#' @details This function can be used to integrate size - if meaningful -
#' @examples
#'
#' bot2$coo[[1]] %>% coo_centpos
#'
#' @family coo_descriptors
#' @export
coo_centpos <- function(x) {
  UseMethod("coo_centpos")
}

#' @export
coo_centpos.default <- function(x) {
  x %>% coo_single() %>% dplyr::summarise_all(mean)
}

#' @export
coo_centpos.list <- function(x) {
  x %>% purrr::map(coo_centpos)
}

#' @export
coo_centpos.coo_tbl <- function(x) {
  x$coo %>%
    purrr::map_df(coo_centpos) %>%
    `colnames<-`(c("centpos_x", "centpos_y")) %>%
    dplyr::bind_cols(x, .)
}


# coo_centsize --------------
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
#' bot2$coo[[1]] %>% coo_centsize
#'
#' @family coo_descriptors
#' @export
coo_centsize <- function(x){
  UseMethod("coo_centsize")
}

#' @export
coo_centsize.default <- function(x) {
  df <- coo_single(x)
  sqrt(mean((df$x-mean(df$x))^2 + (df$y-mean(df$y))^2))
}

#' @export
coo_centsize.list <- function(x){
  purrr::map(x, coo_centsize)
}

#' @export
coo_centsize.coo_tbl <- function(x){
  x %>% dplyr::mutate(centsize=purrr::map_dbl(x$coo, coo_centsize))
}

