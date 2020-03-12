# coo_center ----------------------------------------------

#' Center shapes
#'
#' Returns a shape centered on the origin.
#'
#' @param x [coo_single], [coo_tbl] or a list of shapes
#' @return [coo_single] or [coo_tbl] or a list of shapes
#' @family coo_modifyers
#' @examples
#'
#' @rdname coo_center
#' @aliases coo_centre
#'
#' @examples
#' bot2$coo[[1]] %>% coo_center
#'
#' @export
coo_center <- function(x) {
  UseMethod("coo_center")
}

#' @export
coo_center.default <- function(x) {
  x %>% scale(scale=FALSE)
}

#' @export
coo_center.list <- function(x){
  x %>% purrr::map(coo_center)
}

#' @export
coo_center.coo_tbl <- function(x) {
  x %>% dplyr::mutate(coo=purrr::map(x$coo, coo_center))
}

#' @rdname coo_center
#' @export
coo_centre <- coo_center


# coo_trans ----------------------------------------------

#' Translate shapes
#'
#' Returns a shape translate by `x` and `y`.
#'
#' @param x [coo_single], [coo_tbl] or a list of shapes
#' @param x_trans `numeric` how much translate on x-axis
#' @param y_trans `numeric` how much translate on y-axis
#' @return [coo_single] or [coo_tbl] or a list of shapes
#' @family coo_modifyers
#' @examples
#'
#' @examples
#' bot2$coo[[1]] %>% coo_trans
#'
#' @export
coo_trans <- function(x, x_trans=0, y_trans=0) {
  UseMethod("coo_trans")
}

#' @export
coo_trans.default <- function(x, x_trans=0, y_trans=0) {
  x %>% coo_single() %>%
    dplyr::mutate(x = .data$x + x_trans, y = .data$y + y_trans)
}

#' @export
coo_trans.list <- function(x, x_trans=0, y_trans=0){
  x %>% purrr::map(coo_trans, x_trans=x_trans, y_trans=y_trans)
}

#' @export
coo_trans.coo_tbl <- function(x, x_trans=0, y_trans=0) {
  x %>% dplyr::mutate(coo=purrr::map(x$coo, coo_trans, x_trans=x_trans, y_trans=y_trans))
}

# coo_scale ----------------------------------------------

#' Scale shapes
#'
#' Returns a scaled shape.
#'
#' @inheritParams coo_center
#' @param scale `numeric` scaling factor ([get_centsize] by default).
#' @return [coo_single] or [coo_tbl] or a list of shapes
#' @family coo_modifyers
#' @examples
#'
#' @examples
#' bot2$coo[[1]] %>% coo_scale
#'
#' @export
coo_scale <- function(x, scale) {
  UseMethod("coo_scale")
}

#' @export
coo_scale.default <- function(x, scale) {
  # use centroid size by default
  if (missing(scale))
    scale <- get_centsize(x)
  # record centroid position to reposition after scaling
  cent <- get_centpos(x)
  x %>%
    # center and scale
    coo_center() %>% `/`(scale) %>%
    # move back to original centroid
    coo_trans(x_trans=cent$x, y_trans=cent$y)
}

#' @export
coo_scale.list <- function(x, scale){
  x <- purrr::map(x, coo_single)
  if (missing(scale))
    scale <- purrr::map_dbl(x, get_centsize)
  purrr::map2(x, scale, coo_scale)
}

#' @export
coo_scale.coo_tbl <- function(x, scale) {
  x %>% dplyr::mutate(coo=purrr::map(x$coo, coo_scale))
}

# coo_align ----------
#' Align shapes
#'
#' Align shape along their longer axis using var-cov matrix and eigen values.
#'
#' @inheritParams coo_center
#' @return [coo_single] or [coo_tbl] or a list of shapes
#' @family coo_modifyers
#' @examples
#' bot2$coo[[5]] %>% coo_align
#' @export
coo_align <- function(x) {
  UseMethod("coo_align")
}

#' @export
coo_align.default <- function(x){
  (as.matrix(x) %*% (svd(stats::var(as.matrix(x)))$u)) %>% coo_single()
}

#' @export
coo_align.list <- function(x){
  x %>% purrr::map(coo_align)
}

#' @export
coo_align.coo_tbl <- function(x){
  x %>% dplyr::mutate(coo=purrr::map(x$coo, coo_align))
}


