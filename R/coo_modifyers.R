#todo: all .list methods should return 'coo_list"


# TRANSLATION AND CO --------------------------------------
# coo_center ----------------------------------------------

#' Center shapes
#'
#' Returns a shape centered on the origin.
#'
#' @param x [coo_single], [coo_list] or [mom_tbl]
#' @param from_col,to_col colnames from where to get the [coo_list]
#' and how to name the resulting one (only for [mom_tbl] method)
#' @param ... useless here
#' @return a [coo_single], [coo_list] or [mom_tbl]
#' @family coo_modifyers
#' @family translations
#' @examples
#'
#' @rdname coo_center
#' @aliases coo_centre
#'
#' @examples
#' bot %>% pick(1) %>% coo_center %>% gg()
#' bot %>% coo_center %>% pile()
#'
#' @export
coo_center <- function(x, from_col=coo, to_col=coo, ...) {
  UseMethod("coo_center")
}

#' @export
coo_center.default <- function(x, ...){
  not_defined("coo_center")
}

#' @describeIn coo_center coo_single method
#' @export
coo_center.coo_single <- function(x, ...) {
  x %>% scale(scale=FALSE) %>% coo_single()
}

#' @describeIn coo_center list method
#' @export
coo_center.coo_list <- function(x, ...){
  x %>% purrr::map(coo_center) %>% coo_list()
}

#' @describeIn coo_center mom_tbl method
#' @export
coo_center.mom_tbl <- function(x, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_center())
}

# # For my english pals
# #' @rdname coo_center
# #' @export
# coo_centre <- coo_center


# coo_trans ----------------------------------------------

#' Translate shapes
#'
#' Returns a shape translated by `x_trans` and `y_trans`.
#'
#' @inherit coo_center params return
#' @param x_trans,y_trans `numeric` how much translate on each direction
#' @family coo_modifyers
#' @family translations
#' @examples
#'
#' bot %>% pick() %>% coo_center() %>% coo_trans(5, 5) %>% gg()
#'
#' @export
coo_trans <- function(x, x_trans=0, y_trans=0, from_col, to_col, ...) {
  UseMethod("coo_trans")
}

#' @export
coo_trans.default <- function(x,  ...) {
  not_defined("coo_trans")
}

#' @export
coo_trans.coo_single <- function(x, x_trans=0, y_trans=0, ...) {
  x %>%
    dplyr::mutate(x = .data$x + x_trans,
                  y = .data$y + y_trans) %>%
    coo_single()
}


#' @export
coo_trans.coo_list <- function(x, x_trans=0, y_trans=0, ...){
  x %>%
    purrr::map(coo_trans,
               x_trans=x_trans,
               y_trans=y_trans) %>%
    coo_list()
}

#' @export
coo_trans.mom_tbl <- function(x, x_trans=0, y_trans=0, from_col=coo, to_col={{from_col}}, ...) {

  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_trans(x_trans=x_trans, y_trans=y_trans))
}

# SCALING AND CO ----------------
# coo_scale ----------------------------------------------

#' Scale shapes
#'
#' Returns a scaled shape.
#'
#' @inherit coo_center params return
#' @param scale `numeric` scaling factor ([get_centsize] by default).
#' @family coo_modifyers
#' @family scalings
#' @examples
#'
#' @examples
#' x <- bot %>% pick(1)
#' x %>% gg()
#' x %>% coo_scale() %>% gg()
#' x %>% coo_scale_x(5) %>% gg()
#' x %>% coo_scale_y(5) %>% gg()
#' @export
coo_scale <- function(x, scale, from_col, to_col, ...) {
  UseMethod("coo_scale")
}

#' @export
coo_scale.default <- function(x, ...){
  not_defined("coo_scale")
}

#' @export
coo_scale.coo_single <- function(x, scale, ...) {
  # use centroid size by default
  if (missing(scale))
    scale <- get_centsize(x)
  # record centroid position to reposition after scaling
  cent <- get_centpos(x)
  # center and scale
  (coo_center(x)/scale) %>%  # keep braces !
    coo_single() %>%         # because stripped by `/` (!)
    # move back to original centroid
    coo_trans(x_trans=cent$x, y_trans=cent$y) %>%
    coo_single()
}

#' @export
coo_scale.coo_list <- function(x, scale, ...){
  if (missing(scale))
    scale <- purrr::map_dbl(x, get_centsize)
  purrr::map2(x, scale, ~coo_scale(.x, scale=.y)) %>%
    coo_list()
}

#' @export
coo_scale.mom_tbl <- function(x, scale, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_scale())
}


# coo_scale_x ----------------------------------------------

#' @describeIn coo_scale scale along x-axis
#' @export
coo_scale_x <- function(x, scale, from_col, to_col, ...) {
  UseMethod("coo_scale_x")
}

#' @export
coo_scale_x.default <- function(x, ...){
  not_defined("coo_scale_x")
}

#' @export
coo_scale_x.coo_single <- function(x, scale, ...) {
  if (missing(scale))
    stop("coo_scale_x: scale is missing")
  # prepare affine transformation matrix
  smat <- matrix(c(scale, 0,
                   0, 1), nrow = 2)
  (as.matrix(x) %*% smat) %>% coo_single()
}

#' @export
coo_scale_x.coo_list <- function(x, scale, ...) {
  if (missing(scale))
    stop("coo_scale_x: scale is missing")
  x %>% purrr::map(coo_scale_x, scale=scale) %>% coo_list()
}

#' @export
coo_scale_x.mom_tbl <- function(x, scale, from_col=coo, to_col={{from_col}}, ...) {
  if (missing(scale))
    stop("coo_scale_x: scale is missing")
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_scale_x(scale=scale))
}

# coo_scale_y ----------------------------------------------

#' @describeIn coo_scale scale along y-axis
#' @export
coo_scale_y <- function(x, scale, from_col, to_col, ...) {
  UseMethod("coo_scale_y")
}

#' @export
coo_scale_y.default <- function(x, ...){
  not_defined("coo_scale_y")
}

#' @export
coo_scale_y.coo_single <- function(x, scale, ...) {
  if (missing(scale))
    stop("coo_scale_y: scale is missing")
  # prepare affine transformation matrix
  smat <- matrix(c(1, 0,
                   0, scale), nrow = 2)
  (as.matrix(x) %*% smat) %>% coo_single()
}

#' @export
coo_scale_y.coo_list <- function(x, scale, ...) {
  if (missing(scale))
    stop("coo_scale_y: scale is missing")
  x %>% purrr::map(coo_scale_y, scale=scale) %>% coo_list()
}

#' @export
coo_scale_y.mom_tbl <- function(x, scale, from_col=coo, to_col={{from_col}}, ...) {
  if (missing(scale))
    stop("coo_scale_y: scale is missing")
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_scale_y(scale=scale))
}

# coo_template ----------------------------------------------

#' Templates shapes
#'
#' Centers shape and scale them so that they are inscribed in a `size`-side square.
#'
#' @inherit coo_center params return
#' @param size `numeric` the side of the square inscribing the shape
#' @family coo_modifyers
#' @family scalings
#' @examples
#' bot %>% pick(1) %>% coo_template() %>% gg()
#' bot %>% coo_template %>% pile()
#' @export
coo_template <- function(x, size=1, from_col, to_col, ...) {
  UseMethod("coo_template")
}

#' @export
coo_template.default <- function(x, ...){
  not_defined("coo_template")
}

#' @export
coo_template.coo_single <- function(x, size=1, ...) {
  # get the rescaling ratio
  k <- min(size/get_diffrange(x))
  # center and apply it
  x %>%
    coo_center %>%
    dplyr::mutate(x=.data$x*k, y=.data$y*k) %>%
    coo_single()
}

#' @describeIn coo_template list method
#' @export
coo_template.coo_list <- function(x, size=1, ...){
  x %>% purrr::map(coo_template, size=size) %>% coo_list()
}


#' @describeIn coo_template mom_tbl method
#' @export
coo_template.mom_tbl <- function(x, size=1, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  dplyr::mutate(x,
                !!to_col := x %>%
                  dplyr::pull(!!from_col) %>%
                  coo_template(size=size))
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
# coo_template_relatively.mom_tbl <- function(x) {
#   x %>% mutate(coo=map(coo, coo_template_relatively))
# }

# SHEARING ------------------------------------------------
# coo_shear ----------------------------------------------

#' Shear shapes
#'
#' Returns a sheared shape.
#'
#' @inherit coo_center params return
#' @param x_k,y_k `numeric` shearing factor over x and y axes
#' (default to 0, ie no shearing)
#' @family coo_modifyers
#' @family shearings
#' @examples
#'
#' @examples
#' x <- bot %>% pick(1) %>% coo_center()
#' x %>% gg()
#' x %>% coo_shear(x_k =  0.25) %>% gg()
#' x %>% coo_shear(y_k = -0.5) %>% gg()
#' x %>% coo_shear(x_k = 0.25, y_k = -0.5) %>% gg()
#' @export
coo_shear <- function(x, x_k, y_k, from_col, to_col, ...) {
  UseMethod("coo_shear")
}

#' @export
coo_shear.default <- function(x, x_k, y_k,...){
  not_defined("coo_shear")
}

#' @export
coo_shear.coo_single <- function(x, x_k=0, y_k=0, ...) {
  # prepare affine transformation matrix
  smat <- matrix(c(1, y_k,
                   x_k, 1), nrow = 2)
  (as.matrix(x) %*% smat) %>% coo_single()
}

#' @export
coo_shear.coo_list <- function(x, x_k=0, y_k=0,  ...){
  x %>%
    purrr::map(coo_shear, x_k=x_k, y_k=y_k) %>%
    coo_list()
}

#' @export
coo_shear.mom_tbl <- function(x, x_k=0, y_k=0, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_shear(x_k=x_k, y_k=y_k))
}


# ROTATION AND CO -----------------------------------------
# coo_align ----------
#' Align shapes
#'
#' Align shape along their longer axis using var-cov matrix and eigen values.
#'
#' @inherit coo_center params return
#' @family coo_modifyers
#' @family rotations
#'
#' @details (todo) For `coo_align_xax`: ff some shapes are upside-down
#' (or mirror of each others), try redefining a new starting point (eg with coo_slidedirection) before
#' the alignment step. This may solve your problem because coo_calliper orders the `$arr.ind`` used by
#' coo_aligncalliper.
#'
#' @examples
#' bot %>% pick(1) %>% coo_align
#' bot %>% pick(1) %>% coo_align_xax
#' @export
coo_align <- function(x, from_col=coo, to_col=coo, ...) {
  UseMethod("coo_align")
}

#' @export
coo_align.default <- function(x, ...){
  not_defined("coo_align")
}

#' @export
coo_align.coo_single <- function(x, ...){
  (as.matrix(x) %*% (svd(stats::var(as.matrix(x)))$u)) %>% coo_single()
}

#' @export
coo_align.coo_list <- function(x, ...){
  x %>% purrr::map(coo_align) %>% coo_list()
}

#' @export
coo_align.mom_tbl <- function(x, from_col=coo, to_col={{from_col}}, ...){
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_align())
}

# coo_align_xax ----------------------------------------------

#' @describeIn coo_align align the longest axis of the shape along the x-axis
#' @export
coo_align_xax <- function(x, from_col, to_col, ...) {
  UseMethod("coo_align_xax")
}

#' @export
coo_align_xax.default <- function(x, ...){
  not_defined("coo_align_xax")
}

#' @export
coo_align_xax.coo_single <- function(x, ...) {
  # first align
  x <- coo_align(x)
  # then remove diff from x-axis to y centroid position
  y_cp <- get_centpos(x)$y
  # return this beauty
  x %>% coo_trans(x_trans = 0, y_trans = -y_cp) %>% coo_single()
}

#' @export
coo_align_xax.coo_list <- function(x, ...) {
  x %>% purrr::map(coo_align_xax) %>% coo_list()
}

#' @export
coo_align_xax.mom_tbl <- function(x, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_align_xax())
}

# coo_rotate ----------------------------------------------
#' Rotate shapes
#'
#' Rotates the coordinates by a `theta` angle (in radians) in
#' the trigonometric direction (anti-clockwise).
#' @inherit coo_center params return
#' @param theta `numeric` angle to rotate (in radians) and in the trigonometric direction (anti-clockwise). Default to `0`.
#' @family coo_modifyers
#' @family rotations
#' @examples
#' x <- bot %>% pick(1)
#' gg(x)
#'
#' x %>% coo_rotate(pi/2) %>% draw(col="red")
#' x %>% coo_rotate(degrees_to_radians(-45)) %>% draw(col="blue")
#'
#' bot %>% coo_rotate(pi) %>% pile()
#'
#' @export
coo_rotate <- function(x, theta = 0, from_col=coo, to_col=coo, ...) {
  UseMethod("coo_rotate")
}

#' @export
coo_rotate.default <- function(x, ...){
  not_defined("coo_rotate")
}

#' @export
coo_rotate.coo_single <- function(x, theta = 0, ...) {
  mat <- matrix(c(cos(-theta), sin(-theta), -sin(-theta), cos(-theta)), nrow = 2)
  x %>% as.matrix() %*% mat %>% coo_single()
}

#' @export
coo_rotate.coo_list <- function(x, theta = 0, ...) {
  mat <- matrix(c(cos(-theta), sin(-theta), -sin(-theta), cos(-theta)), nrow = 2)
  x %>% purrr::map(~.x %>% as.matrix() %*% mat %>% coo_single()) %>% coo_list()
}

#' @export
coo_rotate.list <- coo_rotate.coo_list

#' @describeIn coo_rotate mom_tbl method
#' @export
coo_rotate.mom_tbl<- function(x, theta = 0, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_rotate(theta=theta))
}

# coo_rotatecenter ----------------------------------------
#' Rotate shapes and specify center
#'
#' Rotates the coordinates by a `theta` angle (in radians) in
#' the trigonometric direction (anti-clockwise) and using `center` as coordinates.
#' @inherit coo_center params return
#' @param theta `numeric` angle to rotate (in radians) and in the trigonometric direction (anti-clockwise). Default to `0`.
#' @param center `numeric` of length 2, sepcifying the `(x; y)` coordinates of the rotation center. Default to `c(0, 0)`
#' @family coo_modifyers
#' @family rotations
#' @examples
#' x <- bot %>% pick(5) %>% coo_center() %>% coo_scale()
#' @export
coo_rotatecenter <- function(x, theta=0, center = c(0, 0), from_col=coo, to_col=coo, ...) {
  UseMethod("coo_rotatecenter")
}

#' @export
coo_rotatecenter.default <- function(x, ...){
  not_defined("coo_rotatecenter")
}

#' @export
coo_rotatecenter.coo_single <- function(x, theta=0, center = c(0, 0), ...){
  center <- unlist(center) # if passed as data.frame like
  x %>%
    # probably a more direct option
    coo_trans(x_trans = -center[1], y_trans = -center[2]) %>%
    coo_rotate(theta) %>%
    coo_trans(x_trans = center[1], y_trans = center[2]) %>%
    coo_single()
}

#' @export
coo_rotatecenter.coo_list <- function(x, theta=0, center = c(0, 0), ...) {
  x %>% purrr::map(coo_rotatecenter, center=center) %>% coo_list()
}

#' @export
coo_rotatecenter.mom_tbl <- function(x, theta=0, center = c(0, 0), from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  from_col <- enquo(from_col)
  # here, ensures that if to_col is not provided, it is from_coo too
  if (missing(to_col))
    to_col   <- enquo(from_col)
  else
    to_col <- enquo(to_col)
  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_rotatecenter(theta=theta, center=center) %>%
                        coo_list())
}
# REFLECTING ------------------------------------------------
# coo_reflect_x ----------------------------------------------
#' Reflect shapes
#'
#' Reflects shapes about the x- or the y- axis
#'
#'
#' @inherit coo_center params return
#' @family coo_modifyers
#' @examples
#'
#' bot %>% pick(1) %>% coo_reflect_x() %>% gg()
#' bot %>% pick(1) %>% coo_reflect_y() %>% gg()
#' @name coo_reflect
NULL

#' @describeIn coo_reflect reflects about the x-axis
#' @export
coo_reflect_x <- function(x, from_col, to_col, ...) {
  UseMethod("coo_reflect_x")
}

#' @export
coo_reflect_x.default <- function(x, ...){
  not_defined("coo_reflect_x")
}

#' @export
coo_reflect_x.coo_single <- function(x, ...) {
  m <- matrix(c(1, 0, 0, -1), nrow = 2)
  (as.matrix(x) %*% m) %>% coo_single()
}

#' @export
coo_reflect_x.coo_list <- function(x, ...) {
  x %>% purrr::map(coo_reflect_x) %>% coo_list()
}

#' @export
coo_reflect_x.mom_tbl <- function(x, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_reflect_x())
}

#' @describeIn coo_reflect reflects about the y-axis
#' @export
coo_reflect_y <- function(x, from_col, to_col, ...) {
  UseMethod("coo_reflect_y")
}

#' @export
coo_reflect_y.default <- function(x, ...){
  not_defined("coo_reflect_y")
}

#' @export
coo_reflect_y.coo_single <- function(x, ...) {
  m <- matrix(c(-1, 0, 0, 1), nrow = 2)
  (as.matrix(x) %*% m) %>% coo_single()
}

#' @export
coo_reflect_y.coo_list <- function(x, ...) {
  x %>% purrr::map(coo_reflect_y) %>% coo_list()
}

#' @export
coo_reflect_y.mom_tbl <- function(x, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_reflect_y())
}

# SAMPLING AND CO -----------------------------------------
# coo_sample ----------
#' Sample shapes
#'
#' Changes the number of shape coordinates.
#'
#' [coo_sample] will return coordinates regularly sampled along the curvilinear abscissa.
#' The last point will be dropped so that the distance (new last - first) roughly equals
#' all other distances between consecutive points. This is typically useful for outlines.
#' coo_sample_curve (todo link) will preserve the first and last points.
#' This is typically useful for curves, hence the name.
#' [coo_interpolate] will upsample the number of points.
#' All functions have a `_prop` ally, where `n` is deduced from the proportion you want to retain.
#'
#' @inherit coo_center params return
#' @param n `integer` desired number of coordinates (required)
#' @param prop `numeric` desired poportion of sampled coordinates (required).
#' Below 1 will sample, above 1 will interpolate.
#' @family coo_modifyers
#' @family samplers
#' @examples
#' x <- bot %>% pick(1) %>% coo_sample(24)
#' x %>% gg()
#' x %>% coo_interpolate(120) %>% gg()
#'
#' bot$coo[1:2] %>% coo_sample(12) %>% purrr::map_dbl(nrow)
#' bot$coo[1:2] %>% coo_sample(12) %>% coo_interpolate(24) %>% purrr::map_dbl(nrow)
#'
#'
#' @export
coo_sample <- function(x, n, from_col, to_col, ...) {
  UseMethod("coo_sample")
}

#' @export
coo_sample.default <- function(x,  ...) {
  not_defined("coo_sample")
}

#' @export
coo_sample.coo_single <- function(x, n, ...) {
  # early stop if n is missing
  if (missing(n)){
    stop("coo_sample: n must be provided")
  }
  # early return when unchanged must be returned
  if (nrow(x) == n){
    return(x)
  }
  # case where n is too ambitious,
  # so we message and shoft to coo_interpolate
  if (nrow(x) < n) {
    .msg_warning("coo_sample: less coordinates than `n`, using coo_interpolate")
    return(coo_interpolate(x, n))
  }
  # otherwise sampling with seq is a piece of cake
  x[round(seq(1, nrow(x), len = n + 1)[-(n + 1)]), ] %>% coo_single()
}

#' @export
coo_sample.coo_list <- function(x, n, ...) {
  x %>% purrr::map(coo_sample, n) %>% coo_list()
}

#' @export
coo_sample.mom_tbl <- function(x, n, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_sample(n=n))
}


# coo_sample_prop -----------------------------------------
#' @describeIn coo_sample Sample a proportion of coordinates
#' @export
coo_sample_prop <- function(x, prop, from_col=coo, to_col=coo, ...){
  UseMethod("coo_sample_prop")
}

#' @export
coo_sample_prop.default <- function(x, prop, ...){
  not_defined("coo_sample_prop")
}

#' @export
coo_sample_prop.coo_single <- function(x, prop, ...){
  n <- ceiling(nrow(x)*prop)
  coo_sample(x, n) %>% coo_single()
}

#' @export
coo_sample_prop.coo_list <- function(x, prop, ...){
  n <- ceiling(purrr::map_dbl(x, nrow)*prop)
  purrr::map2(x, n, ~coo_sample(.x, .y)) %>% coo_list()
}

#' @export
coo_sample_prop.mom_tbl <- function(x, prop, from_col=coo, to_col={{from_col}}, ...){

  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_sample_prop(prop=prop))
}

# coo_interpolate -----------------------------------------
#' @describeIn coo_sample Interpolates shape coordinates
#' @export
coo_interpolate <- function(x, n, from_col, to_col, ...) {
  UseMethod("coo_interpolate")
}

#' @export
coo_interpolate.default <- function(x, ...){
  not_defined("coo_interpolate")
}

#' @export
coo_interpolate.coo_single <- function(x, n, ...) {
  # early forward if more coordinates than asked
  if (nrow(x) > n){
    .msg_info("coo_interpolate: n was lower than the number of coordinates, coo_sample instead")
    x %>% coo_sample(n) %>% return()
  }
  # early return when unchanged must be returned
  if (nrow(x)==n){
    return(x)
  }
  # interpolate will cut based on perimeter_along
  # with a reference (x) and a target
  # on which we get ideal cutting along, given n
  old_along <- x %>% get_perim_cumsum() %>% unlist()
  new_along <- seq(0, get_perim(x), length = n + 1)[-(n + 1)]

  # we keep the first point and prototype new_x with NAs
  new_x <- tibble::tibble(x=rep(NA_real_, n), y=x)
  new_x[1, ] <- x[1, ]

  # a loop that will find the embrassing coordinates from original shape
  # todo not urgent: optimize
  for (i in 2:n) {
    # k is id before, k+1 will be id after
    k <- max(which(old_along <= new_along[i]))
    # r is where we fall between k and k+1
    r <- (new_along[i] - old_along[k]) / (old_along[k + 1] - old_along[k])
    new_x[i, ] <- edi(x[k, ], x[k + 1, ], r)
  }

  # return this beauty
  new_x %>% coo_single()
}

#' @export
coo_interpolate.coo_list <- function(x, n, ...){
  x %>% purrr::map(coo_interpolate, n) %>% coo_list()
}

#' @export
coo_interpolate.mom_tbl <- function(x, n, from_col=coo, to_col={{from_col}}, ...){

  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>% dplyr::pull(!!from_col) %>% coo_interpolate(n=n))
}

# coo_sample_rr ----------------------------------------------
#' Sample shapes using the regular radius method
#'
#' Sample coordinates with regular angles
#'
#'
#' @inherit coo_center params return
#' @param n `integer` desired number of coordinates (required)
#' @family coo_modifyers
#' @examples
#' bot %>% pick(1)  # todo
#' @export
coo_sample_rr <- function(x, n ,from_col, to_col, ...) {
  UseMethod("coo_sample_rr")
}

#' @export
coo_sample_rr.default <- function(x, ...){
  not_defined("coo_sample_rr")
}

#' @export
coo_sample_rr.coo_single <- function(x, n, ...) {

  # missing, abort
  if (missing(n))
    stop("coo_sample_rr: 'n' must be provided")

  # too ambitious abort
  if (nrow(x) < n)
    stop("coo_sample: less coordinates than `n`, use coo_interpolate first")

  # to make it work
  # todo: directly versed from old Momocs and Juju, optimize
  x <- as.matrix(x)
  Rx <- x[, 1]
  Ry <- x[, 2]
  le <- length(Rx)
  M <- matrix(c(Rx, Ry), le, 2)
  M1 <- matrix(c(Rx - mean(Rx), Ry - mean(Ry)), le, 2)
  V1 <- complex(real = M1[, 1], imaginary = M1[, 2])
  M2 <- matrix(c(Arg(V1), Mod(V1)), le, 2)
  V2 <- NA
  for (i in 0:(n - 1)) {
    V2[i + 1] <- which.max((cos(M2[, 1] - 2 * i * pi/n)))
  }
  V2 <- sort(V2)
  M1[V2, ] %>% coo_single() %>% return()
}

#' @export
coo_sample_rr.coo_list <- function(x, n, ...) {
  x %>% purrr::map(coo_sample_rr, n=n) %>% coo_list()
}

#' @export
coo_sample_rr.mom_tbl <- function(x, n, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_sample_rr(n=n))
}

# SMOOTHING -----------------------------------------------

# coo_smooth ----------------------------------------------
#' Smooth shapes
#'
#' Smooth shape coordinates using a moving average
#'
#'
#' @inherit coo_center params return
#' @param n `integer` smoothing iterations
#' @family coo_modifyers smooth
#' @examples
#'
#' bot %>% pick(1) %>% coo_smooth(5) %>% gg()
#'
#' @export
coo_smooth <- function(x, n, from_col, to_col, ...) {
  UseMethod("coo_smooth")
}

#' @export
coo_smooth.default <- function(x, ...){
  not_defined("coo_smooth")
}

#' @export
coo_smooth.coo_single <- function(x, n, ...) {
  if (missing(n)){
    .msg_info("coo_smooth: 'n' must be provided")
    stop()
  }
  p <- nrow(x)
  a <- 0
  while (a < n) {
    a <- a + 1
    x_i <- rbind(x[-1, ], x[1, ])
    x_s <- rbind(x[p, ], x[-p, ])
    x <- x/2 + x_i/4 + x_s/4
  }

  # return this beauty
  x %>% coo_single()
}

#' @export
coo_smooth.coo_list <- function(x, n, ...) {
  x %>% purrr::map(coo_smooth, n=n) %>% coo_list()
}

#' @export
coo_smooth.mom_tbl <- function(x, n, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_smooth(n=n))
}

# coo_smooth_curve ----------------------------------------------
#' Smooth shapes but preserve tips
#'
#' [coo_smooth] variant that preserves coordinates of first and alst points.
#' Typically useful for curves
#'
#'
#' @inherit coo_center params return
#' @param n `integer` smoothing iterations
#' @family coo_modifyers smooth
#' @examples
#'
#' bot %>% pick(1) #todo
#'
#'
#' @export
coo_smooth_curve <- function(x, n, from_col, to_col, ...) {
  UseMethod("coo_smooth_curve")
}

#' @export
coo_smooth_curve.default <- function(x, ...){
  not_defined("coo_smooth_curve")
}

#' @export
coo_smooth_curve.coo_single <- function(x, n, ...) {
  if (missing(n)){
    .msg_info("coo_smooth: 'n' must be provided")
    stop()
  }

  p <- nrow(x)
  a <- 0
  while (a < n) {
    a <- a + 1
    for (i in 2:(p - 1)) {
      x[i, ] <- (x[i - 1, ] * 0.25 + x[i, ] * 0.5 + x[i + 1, ] * 0.25)
    }
  }

  # return this beauty
  x %>% coo_single()
}

#' @export
coo_smooth_curve.coo_list <- function(x, n, ...) {
  x %>% purrr::map(coo_smooth_curve, n=n) %>% coo_list()
}

#' @export
coo_smooth_curve.mom_tbl <- function(x, n, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_smooth_curve(n=n))
}

# CLOSING AND OPENING -------------------------------------

# coo_close ----------------------------------------------

#' Close and unclose shapes
#'
#' A closed shape is a shape with the same first and last coordinates.
#'
#'
#' @inherit coo_center params return
#' @family coo_modifyers close
#' @examples
#'
#' bot %>% pick(1)  # todo
#' @rdname coo_close
#' @export
coo_close <- function(x, from_col, to_col, ...) {
  UseMethod("coo_close")
}

#' @export
coo_close.default <- function(x, ...){
  not_defined("coo_close")
}

#' @export
coo_close.coo_single <- function(x, ...) {
  # if already closed, just forward
  if (is_closed(x))
    x
  else
    dplyr::bind_rows(x, x[1, ]) %>% coo_single()
}

#' @export
coo_close.coo_list <- function(x, ...) {
  x %>% purrr::map(coo_close) %>% coo_list()
}

#' @export
coo_close.mom_tbl <- function(x, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_close())
}

# coo_unclose ---------------------------------------------
#' @describeIn coo_close Unclose shapes
#' @export
coo_unclose <- function(x, from_col, to_col, ...) {
  UseMethod("coo_unclose")
}

#' @export
coo_unclose.default <- function(x, ...){
  not_defined("coo_unclose")
}

#' @export
coo_unclose.coo_single <- function(x, ...) {
  # if already unclosed, just forward
  if (is_unclosed(x))
    x
  else
    x[-nrow(x), ] %>% coo_single()
}

#' @export
coo_unclose.coo_list <- function(x, ...) {
  x %>% purrr::map(coo_unclose) %>% coo_list()
}

#' @export
coo_unclose.mom_tbl <- function(x, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_unclose())
}

# testers -------------------------------------------------
#' @describeIn coo_close Tests if a coo_single (only) is closed
#' @export
is_closed <- function(x){
  identical(x[1, ], x[nrow(x), ])
}


#' @describeIn coo_close Tests if a coo_single (only) is unclosed
#' @export
is_unclosed <- function(x){
  !is_closed(x)
}

# coo_up ----------------------------------------------
#' Retains coordinates based on their x/y sign
#'
#' Useful for centered or aligned shapes.
#'
#' * [coo_up] retains only coordinates with `y >= 0`
#' * [coo_down] retains only coordinates with `y <= 0`
#' * [coo_left] retains only coordinates with `x <= 0`
#' * [coo_right] retains only coordinates with `x >= 0`
#'
#' @inherit coo_center params return
#' @family coo_modifyers
#' @examples
#'
#' bot %>% pick(1) %>% coo_center() %>% coo_left() %>% gg()
#'
#' @name coo_up
NULL

#' @describeIn coo_up filter upper part of a shape
#' @export
coo_up <- function(x, from_col, to_col, ...) {
  UseMethod("coo_up")
}

#' @export
coo_up.default <- function(x, ...){
  not_defined("coo_up")
}

#' @export
coo_up.coo_single <- function(x, ...) {
  x %>% dplyr::filter(.data$y>=0) %>% coo_single()
}

#' @export
coo_up.coo_list <- function(x, ...) {
  x %>% purrr::map(coo_up) %>% coo_list()
}

#' @export
coo_up.mom_tbl <- function(x, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_up())
}

# coo_down ------------------------------------------------
#' @describeIn coo_up filter lower part of a shape
#' @export
coo_down <- function(x, from_col, to_col, ...) {
  UseMethod("coo_down")
}

#' @export
coo_down.default <- function(x, ...){
  not_defined("coo_down")
}

#' @export
coo_down.coo_single <- function(x, ...) {
  x %>% dplyr::filter(.data$y<=0) %>% coo_single()
}

#' @export
coo_down.coo_list <- function(x,  ...) {
  x %>% purrr::map(coo_down) %>% coo_list()
}

#' @export
coo_down.mom_tbl <- function(x, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_down())
}

# coo_left ------------------------------------------------
#' @describeIn coo_up filter left part of a shape
#' @export
coo_left <- function(x, from_col, to_col, ...) {
  UseMethod("coo_left")
}

#' @export
coo_left.default <- function(x, ...){
  not_defined("coo_left")
}

#' @export
coo_left.coo_single <- function(x, ...) {
  x %>% dplyr::filter(.data$x<=0) %>% coo_single()
}

#' @export
coo_left.coo_list <- function(x,  ...) {
  x %>% purrr::map(coo_left) %>% coo_list()
}

#' @export
coo_left.mom_tbl <- function(x,from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_left())
}

# coo_right ------------------------------------------------
#' @describeIn coo_up filter right part of a shape
#' @export
coo_right <- function(x, from_col, to_col, ...) {
  UseMethod("coo_right")
}

#' @export
coo_right.default <- function(x, ...){
  not_defined("coo_right")
}

#' @export
coo_right.coo_single <- function(x, ...) {
  x %>% dplyr::filter(.data$x<=0) %>% coo_single()
}

#' @export
coo_right.coo_list <- function(x, ...) {
  x %>% purrr::map(coo_right) %>% coo_list()
}

#' @export
coo_right.mom_tbl <- function(x, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_right())
}

# coo_rev ----------------------------------------------
#' Reverse coordinates
#'
#' XXX coordinates.
#'
#'
#' @inherit coo_center params return
#' @family coo_modifyers
#' @examples
#'
#' bot %>% coo_sample(12) %>% gg()
#' bot %>% coo_sample(12) %>% coo_rev() %>% gg()
#' @export
coo_rev <- function(x, from_col, to_col, ...) {
  UseMethod("coo_rev")
}

#' @export
coo_rev.default <- function(x, ...){
  not_defined("coo_rev")
}

#' @export
coo_rev.coo_single <- function(x, ...) {
  x[nrow(x):1, ] %>% coo_single()
}

#' @export
coo_rev.coo_list <- function(x, ...) {
  x %>% purrr::map(coo_rev) %>% coo_list()
}

#' @export
coo_rev.mom_tbl <- function(x, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_rev())
}

# coo_trim ----------------------------------------------
#' Trim coordinates from shape
#'
#' [coo_trim_head] removes the first `n` coordinates from shape,
#' [coo_trim_tail] removes the last `n` coordinates,
#' [coo_trim] does both.
#'
#'
#' @inherit coo_center params return
#' @param n `integer` how many coordinates shall we trim
#' @family coo_modifyers
#' @examples
#'
#' x <- bot %>% pick(1) %>% coo_sample(12)
#'
#' coo_trim(x, 5)
#' coo_trim_head(x, 5)
#' coo_trim_tail(x, 5)
#' @export
coo_trim <- function(x, n, from_col, to_col, ...) {
  UseMethod("coo_trim")
}

#' @export
coo_trim.default <- function(x, ...){
  not_defined("coo_trim")
}

#' @export
coo_trim.coo_single <- function(x, n, ...) {
  if (missing(n))
    stop("coo_trim: `n` is missing")

  x[(n+1):(nrow(x)-n), ] %>% coo_single()
}

#' @export
coo_trim.coo_list <- function(x, n, ...) {
  x %>% purrr::map(coo_trim, n) %>% coo_list()
}

#' @export
coo_trim.mom_tbl <- function(x, n, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_trim(n))
}

# coo_trim_head -------------
#' @describeIn coo_trim Trims head of shape
#' @export
coo_trim_head <- function(x, n, from_col, to_col, ...) {
  UseMethod("coo_trim_head")
}

#' @export
coo_trim_head.default <- function(x, ...){
  not_defined("coo_trim_head")
}

#' @export
coo_trim_head.coo_single <- function(x, n, ...) {
  if (missing(n))
    stop("coo_trim_head: `n` is missing")

  x[(n+1):nrow(x), ] %>% coo_single()
}

#' @export
coo_trim_head.coo_list <- function(x, n, ...) {
  x %>% purrr::map(coo_trim_head, n) %>% coo_list()
}

#' @export
coo_trim_head.mom_tbl <- function(x, n, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_trim_head(n))
}

# coo_trim_tail -------------
#' @describeIn coo_trim Trims tail of shape
#' @export
coo_trim_tail <- function(x, n, from_col, to_col, ...) {
  UseMethod("coo_trim_tail")
}

#' @export
coo_trim_tail.default <- function(x, ...){
  not_defined("coo_trim_tail")
}

#' @export
coo_trim_tail.coo_single <- function(x, n, ...) {
  if (missing(n))
    stop("coo_trim_tail: `n` is missing")

  x[(n+1):nrow(x), ] %>% coo_single()
}

#' @export
coo_trim_tail.coo_list <- function(x, n, ...) {
  x %>% purrr::map(coo_trim_tail, n) %>% coo_list()
}

#' @export
coo_trim_tail.mom_tbl <- function(x, n, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_trim_tail(n))
}

# THOSE USING LDK -----------------------------------------

# coo_slide -----------------------------------------------
#' Slide coordinates
#'
#' Slides the coordinates so that the id-th coordinate,
#' or a particular landmark, become the first one.

#' @inherit coo_center return
#'
#' @param x [coo_single], [coo_list] or [mom_tbl]
#' @param id `integer` which is (column row) should be used as the first point
#' @param ldk `integer` which landkmark should be used as the first point
#' @param from_col,ldk_col colnames from where to get the [coo_list]
#' @param to_col colname where to set the result (default to `from_col`)
#' and how to name the resulting one (only for [mom_tbl] method)
#' @param ... useless here
#'
#' @details
#'
#' For [mom_tbl] objects, three different flavours exist:
#'
#' * no ldk passed and a single id is passed: all id-th points
#' within the shapes will become the first points.
#' * no ldk passed and a vector of ids of `nrow(x)`: for every shape,
#' the id-th point will be used as the id-th point.
#' * a single ldk is passed: the ldk-th ldk will be used to slide every shape.
#' If an id is (also) passed, it is ignored with a message.
#' See examples.
#'
#' `ldk` handling is only supported on `mom_tbl` objects.
#'
#' @examples
#' h <- hearts %>% dplyr::slice(1:5) # for speed sake
#' pile(h)
#'
#' h %>% coo_slide(ldk=1) %>% pile()
#' @family coo_modifyers
#' @family slide
#' @export
coo_slide <- function(x, id, ldk, from_col=coo, ldk_col=ldk, to_col={{from_col}}, ...) {
  UseMethod("coo_slide")
}

#' @export
coo_slide.default <- function(x, ...) {
  not_defined("coo_slide")
}

#' @export
coo_slide.coo_single <- function(x, id, ...){
  if (missing(id))
    stop("coo_slide: id must be provided")

  # id should be of length 1
  if (length(id)>1){
    .msg_warning("coo_slide: id must be of length 1. Retaining first ({id[1]})")
    id <- id[1]
  }

  # check too ambitious id
  n <- nrow(x)
  if (id > n){
    .msg_warning("coo_slide: id must be <= nrow(x). Using n")
    id <- n
  }

  # here we go
  slided_ids <- c(id:n, 1:(id - 1))
  return(x[slided_ids, ])
}

#' @export
coo_slide.coo_list <- function(x, id, ...){
  if (missing(id))
    stop("coo_slide: id must be provided")

  # recycle common if not already
  l <- length(x)

  # single id passed
  if (length(id)==1){
    # .msg_info("coo_slide: id was recycled to length {l}") # just annoying I guess
    id <- rep(id, l)
  }
  # check that if no single id, correct lenfth
  if (length(id) != l)
    stop("coo_slide: id must be of length {l} or 1")

  purrr::map2(x, id, ~ coo_slide(.x, id=.y)) %>% coo_list()
}


#' @export
coo_slide.mom_tbl <- function(x, id, ldk, from_col=coo, ldk_col=ldk, to_col={{from_col}},...){
  # tidyeval
  c(from_col, ldk_col) %<-% tidyeval_coo_and_ldk({{from_col}}, {{ldk_col}})
  to_col <- enquo(to_col)

  # ldk case, handles ldk_col
  if (provided(id) && provided(ldk))
    .msg_info("coo_slide: id and ldk provided. Only ldk is used")
  # when id is provided, id is just id
  if (missing(id) | provided(ldk)){
    if (missing(ldk) & !col_present(x, ldk)){
      .msg_info("coo_slide: id not provided, and {as_name(ldk_col)} not present")
      stop()
    } else {
      # ldk_col present, extract ldk-th for each
      .msg_info("coo_slide: id not provided, working on {as_name(ldk_col)}")
      id <- x %>% dplyr::pull(!!ldk_col) %>% purrr::map_dbl(~.x[ldk])
    }
  }

  # operate
  dplyr::mutate(x, {{to_col}} := x %>%
                  dplyr::pull(!!from_col) %>%
                  coo_slide(id=id))
}

# coo_split -----------------------------------------------
#' Split shapes on speficied coordinates
#'
#' Take a shape with `n` coordinates. For each `id` or `ldk` passed,
#' the shape will be splitted on the corresponding coordinates
#' and (`length(id/ldk)+1`) fragments returned.
#'
#'
#' @inherit coo_slide params return
#' @param share `logical`  whether to share the slicing coordinate between successive fragments.
#'
#' @details
#'
#' If `share=TRUE`, then each slicing coordinates will be shared between consecutive fragments,
#' that is the last coordinates of the `n-th` fragment will also be
#' the first of the `n+1-th` fragment. That is usually what you want, and thus default to `TRUE`.
#'
#' `ldk` handling is only supported on `mom_tbl` objects.
#'
#' @note
#' `ldk` happens to be present for `coo_single` and `coo_list` methods (unlike `coo_slide` for instance),
#' only to please R CMD CHECK S3 consistency _and_
#' maintain a sensible order for arguments, with `share` after `coo` and `ldk`.
#' For these classes, it is ignored with a message.
#'
#' @seealso
#' Have a look to coo_slidegap (todo link when ready) if you have problems with gaps
#' after slicing around landmarks and/or starting points.
#'
#' @family coo_modifyers
#' @family split
#' @examples
#'
#' x <- bot %>% pick(1) %>% coo_sample(12)
#' x %>% coo_split(id=c(4, 8))
#' x %>% coo_split(id=c(4, 8), share=FALSE)
#'
#' hearts %>%
#'   dplyr::slice(1:2) %>% # for the sake of speed
#'   coo_split(ldk=2:3)
#'
#' # then dplyr::rename or Momocs2::coo_select if you want to rename/select columns
#' @export
coo_split <- function(x, id, ldk, share, from_col=coo, ldk_col=ldk, to_col={{from_col}}, ...) {
  UseMethod("coo_split")
}

#' @export
coo_split.default <- function(x, ...) {
  not_defined("coo_split")
}

#' @export
coo_split.coo_single <- function(x, id, ldk, share=TRUE, ...){
  # ldk would be avoided for this method
  # but R CMD CHECK is not happy with the idea (S3 consistency)
  if (provided(ldk))
    .msg_info("coo_split.coo_list: ignores ldk")

  # mandatory checking
  if (missing(id)){
    stop("coo_split: id must be provided")
  }

  n <- nrow(x)
  if (any(id>n)){
    .msg_info("coo_split: {sum(id > n)} id were > nrow(x). Using remaining ids")
    id <- id[id <= n]
  }

  # test if some remain
  if (length(id)==0){
    stop("coo_split: at least one id is expected")
  }

  # here we go: we prepare a table
  split_tbl <- dplyr::transmute(x, x=NA_integer_)

  # fill ending partitions
  split_tbl[id, ] <- seq_along(id)
  # and their beginning
  split_tbl[id+1, ] <- seq_along(id)+1
  # tidyr::fill will finish the job
  split_tbl %>%
    tidyr::fill(x, .direction="updown") %>%
    dplyr::slice(1:n) %>%
    # pull this beauty
    dplyr::pull() %>%
    # split x with it
    split(x, .) -> res

  # sew back last point
  if (share & length(res)>1){
    # from the second (n) fragment, paste back the last coordinates of (n-1)
    purrr::map(2:length(res),
               ~dplyr::bind_rows(
                 dplyr::slice(res[[.x-1]], dplyr::n()),
                 res[[.x]]
               )
    ) %>% c(res[1], .) -> res # and dont forget the 'untouched) first fragment
  }
  # miniminally name fragments and return this beauty
  res %>% purrr::set_names(seq_along(res))
}

#' @export
coo_split.coo_list <- function(x, id, ldk, share=TRUE, ...){
  # ldk would be avoided for this method
  # but R CMD CHECK is not happy with the idea (S3 consistency)
  if (provided(ldk))
    .msg_info("coo_split.coo_list: ignores ldk")

  if (missing(id))
    stop("coo_split: id must be provided")

  # recycle common if not already
  l <- length(x)

  # since id length can be > 1, turn to list
  # so that we can peacefully map2 and also recycle coo_slide code
  if (!is.list(id))
    id <- list(id)

  # single id passed
  if (length(id)==1){
    .msg_info("coo_split: id was recycled to length {l}")
    id <- rep(id, l)
  }
  # check that if no single id, correct lenfth
  if (length(id) != l)
    stop("coo_split: id must be of length {l} or 1")

  # return these beauties
  purrr::map2(x, id, ~ coo_split(.x, id=unlist(.y), share=share))
}


#' @export
coo_split.mom_tbl <- function(x, id, ldk, share=TRUE, from_col=coo, ldk_col=ldk, to_col={{from_col}},...){
  # tidyeval
  c(from_col, ldk_col) %<-% tidyeval_coo_and_ldk({{from_col}}, {{ldk_col}})
  to_col <- enquo(to_col)

  # ldk case, handles ldk_col
  if (provided(id) && provided(ldk))
    .msg_info("coo_split: id and ldk provided. Only ldk is used")
  # when id is provided, id is just id
  if (missing(id) | provided(ldk)){
    if (missing(ldk) & !col_present(x, ldk)){
      .msg_info("coo_split: id not provided, and {as_name(ldk_col)} not present")
      stop()
    } else {
      # ldk_col present, extract ldk-th for each
      .msg_info("coo_split: id not provided, working on {as_name(ldk_col)}")
      id <- x %>% dplyr::pull(!!ldk_col) %>% purrr::map(~.x[ldk])
    }
  }

  # operate, in more steps

  # we first call coo_split.coo_list on the concerned list
  res <- x %>% dplyr::pull(!!from_col) %>% coo_split(id=id, share=share)

  # then
  # ~ 3.5 times faster to declare coo_list and mom afterwards
  res <- res %>%
    purrr::map(~ .x %>%
                 purrr::map(list) %>%
                 # and make a mom from each
                 tibble::as_tibble()) %>%
    dplyr::bind_rows() %>%
    purrr::modify(coo_list) %>%
    mom()

  # then we define explicit colnames
  colnames(res) <- paste0(as_name(to_col), "_", seq_along(res))

  # finally, bind these beauties back to x and return
  dplyr::bind_cols(x, res)
}
