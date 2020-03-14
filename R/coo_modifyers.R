#todo: all .list methods should return 'coo_list"

# TRANSLATION AND CO --------------------------------------
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
#' bot2 %>% pick(1) %>% coo_center %>% gg()
#' bot2 %>% coo_center %>% pile()
#'
#' @export
coo_center <- function(x) {
  UseMethod("coo_center")
}

#' @export
coo_center.default <- function(x) {
  x %>% scale(scale=FALSE) %>% coo_single()
}

#' @export
coo_center.list <- function(x){
  x %>% purrr::map(coo_center) %>% coo_list()
}

#' @export
coo_center.coo_single <- function(x) {
  x %>% scale(scale=FALSE) %>% coo_single()
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
  x %>% purrr::map(coo_trans, x_trans=x_trans, y_trans=y_trans) %>% coo_list()
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
    coo_trans(x_trans=cent$x, y_trans=cent$y) %>%
    coo_single()
}

#' @export
coo_scale.list <- function(x, scale){
  x <- purrr::map(x, coo_single)
  if (missing(scale))
    scale <- purrr::map_dbl(x, get_centsize)
  purrr::map2(x, scale, coo_scale) %>% coo_list()
}

#' @export
coo_scale.coo_tbl <- function(x, scale) {
  x %>% dplyr::mutate(coo=purrr::map(x$coo, coo_scale))
}

# coo_template ----------------------------------------------

#' XXX shapes
#'
#' Centers shape and scale them so that they are inscribed in a `size`-side square.
#'
#' @param x [coo_single], [coo_list] or [coo_tbl]
#' @param size `numeric` the side of the square inscribing the shape
#' @param ... additional parameters
#' @return templated [coo_list] or [coo_tbl]
#' @family coo_modifyers
#' @examples
#' bot2 %>% pick(1) %>% coo_template() %>% gg()
#' bot2 %>% coo_template %>% pile()
#' @export
coo_template <- function(x, size, ...) {
  UseMethod("coo_template")
}

#' @export
coo_template.default <- function(x, size=1, ...) {
  # get the rescaling ratio
  k <- min(size/get_diffrange(x))
  # center and apply it
  x %>% coo_center %>% dplyr::mutate(x=.data$x*k, y=.data$y*k)
}

#' @export
coo_template.list <- function(x, size=1, ...){
  x %>% purrr::map(coo_template, size=size) %>% coo_list()
}

#' @export
coo_template.coo_tbl <- function(x, size=1, ...) {
  x %>% dplyr::mutate(coo=purrr::map(coo, coo_template, size=size))
}


# coo_template_relatively ----------------------------------------------

# #' @rdname coo_template
# #' @export
# coo_template_relatively <- function(x) {
#   UseMethod("coo_template_relatively")
# }
# #'
# #' #' @export
# #' coo_template_relatively.default <- function(x) {
# #'   .msg_info("no coo_template_relatively method for this class")
# #' }
# #'
# #' #' @export
# #' coo_template_relatively.list <- function(x){
# #'   x %>% map(coo_template_relatively)
# #' }
# #'
# #' #' @export
# #' coo_template_relatively.coo_single <- function(x) {
# #'   x %>% mutate(coo=map(coo, coo_template_relatively))
# #' }
#
# #' @export
# coo_template_relatively.coo_tbl <- function(x) {
#   x %>% mutate(coo=map(coo, coo_template_relatively))
# }

# ROTATION AND CO -----------------------------------------
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
  x %>% purrr::map(coo_align) %>% coo_list()
}

#' @export
coo_align.coo_tbl <- function(x){
  x %>% dplyr::mutate(coo=purrr::map(x$coo, coo_align))
}


# coo_rotate ----------------------------------------------
# helping functions
#' @rdname coo_rotate
#' @export
degrees_to_radians <- function(x){
  x*pi/180
}

#' @rdname coo_rotate
#' @export
radians_to_degrees <- function(x){
  x*180/pi
}

# coo_rotate ----------------------------------------------
#' Rotate shapes
#'
#' Rotates the coordinates by a `theta` angle (in radians) in
#' the trigonometric direction (anti-clockwise).
#' `coo_rotate` uses the origin, but `coo_rotatecenter` allows to specify another center.
#' `degrees_to_radians` and `radians_to_degrees` helps convert between systems.
#'
#' @inheritParams coo_center
#' @param theta `numeric` angle to rotate (in radians) and in the trigonometric direction (anti-clockwise)
#' @param center `numeric` of length 2, sepcifying the `(x; y)` coordinates of the rotation center
#' @return a [coo_single], [coo_list] or [coo_tbl]
#' @examples
#' x <- bot2 %>% pick(1)
#' gg(x)
#'
#' x %>% coo_rotate(pi/2) %>% draw(col="red")
#' x %>% coo_rotate(degrees_to_radians(-45)) %>% draw(col="blue")
#'
#' bot2 %>% coo_rotate(pi) %>% pile()
#'
#' @rdname coo_rotate
#' @export
coo_rotate <- function(x, theta = 0) {
  UseMethod("coo_rotate")
}

#' @export
coo_rotate.default <- function(x, theta = 0) {
  mat <- matrix(c(cos(-theta), sin(-theta), -sin(-theta), cos(-theta)), nrow = 2)
  x %>% as.matrix() %*% mat %>% coo_single()
}

#' @export
coo_rotate.coo_list <- function(x, theta = 0) {
  mat <- matrix(c(cos(-theta), sin(-theta), -sin(-theta), cos(-theta)), nrow = 2)
  x %>% purrr::map(~.x %>% as.matrix() %*% mat %>% coo_single()) %>% coo_list()
}

#' @export
coo_rotate.list <- coo_rotate.coo_list

#' @export
coo_rotate.coo_tbl<- function(x, theta = 0) {
  x %>% dplyr::mutate(coo=coo_rotate(coo, theta=theta))
}

# coo_rotatecenter ----------------------------------------
#' @rdname coo_rotate
#' @export
coo_rotatecenter <- function(x, theta, center = c(0, 0)) {
  UseMethod("coo_rotatecenter")
}

#' @export
coo_rotatecenter.default <- function(x, theta, center = c(0, 0)){
  center <- unlist(center) # if passed as data.frame like
  x %>%
    # probably a more direct option
    coo_trans(x_trans = -center[1], y_trans = -center[2]) %>%
    coo_rotate(theta) %>%
    coo_trans(x_trans = center[1], y_trans = center[2])
}

#' @export
coo_rotatecenter.coo_list <- function(x, theta, center = c(0, 0)) {
  x %>% purrr::map(coo_rotatecenter, center=center) %>% coo_list()
}

#' @export
coo_rotatecenter.list <- coo_rotatecenter.coo_list

#' @export
coo_rotatecenter.coo_tbl <- function(x, theta, center = c(0, 0)) {
  x %>% dplyr::mutate(coo=coo_rotatecenter(coo, center=center))
}


