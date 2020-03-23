#todo: all .list methods should return 'coo_list"

# TRANSLATION AND CO --------------------------------------
# coo_center ----------------------------------------------

#' Center shapes
#'
#' Returns a shape centered on the origin.
#'
#' @param x [coo_single], [coo_list] or [coo_tbl]
#' @param from_col,to_col colnames from where to get the [coo_list]
#' and how to name the resulting one (only for [coo_tbl] method)
#' @param ... useless here
#' @return a [coo_single], [coo_list] or [coo_tbl]
#' @family coo_modifyers
#' @family translations
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
coo_center <- function(x, ...) {
  UseMethod("coo_center")
}

#' @describeIn coo_center default method
#' @export
coo_center.default <- function(x, ...) {
  x %>% scale(scale=FALSE) %>% coo_single()
}

#' @describeIn coo_center list method
#' @export
coo_center.list <- function(x, ...){
  x %>% purrr::map(coo_center) %>% coo_list()
}

#' @describeIn coo_center coo_single method
#' @export
coo_center.coo_single <- function(x, ...) {
  x %>% scale(scale=FALSE) %>% coo_single()
}

#' @describeIn coo_center coo_tbl method
#' @export
coo_center.coo_tbl <- function(x, from_col=coo, to_col=coo, ...) {
  # tidyeval
  from_col <- enquo(from_col)
  # here, ensures that if to_col is not provided, it is from_coo too
  if (missing(to_col))
    to_col   <- enquo(from_col)
  else
    to_col <- enquo(to_col)
  # operate
  x %>% dplyr::mutate(!!to_col := x %>% dplyr::pull(!!from_col) %>% coo_center())
}

# # For my english pals
# #' @rdname coo_center
# #' @export
# coo_centre <- coo_center


# coo_trans ----------------------------------------------

#' Translate shapes
#'
#' Returns a shape translate by `x` and `y`.
#'
#' @inherit coo_center params return
#' @param x_trans,y_trans `numeric` how much translate on each direction
#' @family coo_modifyers
#' @family translations
#' @examples
#' bot2$coo[[1]] %>% coo_trans
#'
#' @export
coo_trans <- function(x, x_trans=0, y_trans=0, ...) {
  UseMethod("coo_trans")
}

#' @describeIn coo_trans default method
#' @export
coo_trans.default <- function(x, x_trans=0, y_trans=0, ...) {
  x %>% coo_single() %>%
    dplyr::mutate(x = .data$x + x_trans, y = .data$y + y_trans)
}

#' @describeIn coo_trans list method
#' @export
coo_trans.list <- function(x, x_trans=0, y_trans=0, ...){
  x %>% purrr::map(coo_trans, x_trans=x_trans, y_trans=y_trans) %>% coo_list()
}

#' @describeIn coo_trans coo_tbl method
#' @export
coo_trans.coo_tbl <- function(x, x_trans=0, y_trans=0, from_col=coo, to_col=coo, ...) {

  # tidyeval
  from_col <- enquo(from_col)
  # here, ensures that if to_col is not provided, it is from_coo too
  if (missing(to_col))
    to_col   <- enquo(from_col)
  else
    to_col <- enquo(to_col)
  # operate
  x %>% dplyr::mutate(!!to_col := x %>% dplyr::pull(!!from_col) %>% coo_trans(x_trans=x_trans, y_trans=y_trans))
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
#' bot2$coo[[1]] %>% coo_scale
#'
#' @export
coo_scale <- function(x, scale, ...) {
  UseMethod("coo_scale")
}

#' @describeIn coo_scale default method
#' @export
coo_scale.default <- function(x, scale, ...) {
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

#' @describeIn coo_scale list method
#' @export
coo_scale.list <- function(x, scale, ...){
  x <- purrr::map(x, coo_single)
  if (missing(scale))
    scale <- purrr::map_dbl(x, get_centsize)
  purrr::map2(x, scale, coo_scale) %>% coo_list()
}

#' @describeIn coo_scale coo_tbl method
#' @export
coo_scale.coo_tbl <- function(x, scale, from_col=coo, to_col=coo, ...) {
  # tidyeval
  from_col <- enquo(from_col)
  # here, ensures that if to_col is not provided, it is from_coo too
  if (missing(to_col))
    to_col   <- enquo(from_col)
  else
    to_col <- enquo(to_col)
  # operate
  x %>% dplyr::mutate(!!to_col := x %>% dplyr::pull(!!from_col) %>% coo_scale())
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
#' bot2 %>% pick(1) %>% coo_template() %>% gg()
#' bot2 %>% coo_template %>% pile()
#' @export
coo_template <- function(x, size, ...) {
  UseMethod("coo_template")
}

#' @describeIn coo_template default method
#' @export
coo_template.default <- function(x, size=1, ...) {
  # get the rescaling ratio
  k <- min(size/get_diffrange(x))
  # center and apply it
  x %>% coo_center %>% dplyr::mutate(x=.data$x*k, y=.data$y*k)
}

#' @describeIn coo_template list method
#' @export
coo_template.list <- function(x, size=1, ...){
  x %>% purrr::map(coo_template, size=size) %>% coo_list()
}


#' @describeIn coo_template coo_tbl method
#' @export
coo_template.coo_tbl <- function(x, size=1, from_col=coo, to_col=coo, ...) {
  # tidyeval
  from_col <- enquo(from_col)
  # here, ensures that if to_col is not provided, it is from_coo too
  if (missing(to_col))
    to_col   <- enquo(from_col)
  else
    to_col <- enquo(to_col)
  # operate
  x %>% dplyr::mutate(!!to_col := x %>% dplyr::pull(!!from_col) %>% coo_template(size=size))
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
#' @inherit coo_center params return
#' @family coo_modifyers
#' @family rotations
#' @examples
#' bot2$coo[[5]] %>% coo_align
#' @export
coo_align <- function(x, ...) {
  UseMethod("coo_align")
}

#' @describeIn coo_align default method
#' @export
coo_align.default <- function(x, ...){
  (as.matrix(x) %*% (svd(stats::var(as.matrix(x)))$u)) %>% coo_single()
}

#' @describeIn coo_align list method
#' @export
coo_align.list <- function(x, ...){
  x %>% purrr::map(coo_align) %>% coo_list()
}

#' @describeIn coo_align coo_tbl method
#' @export
coo_align.coo_tbl <- function(x, from_col=coo, to_col=coo, ...){
  # tidyeval
  from_col <- enquo(from_col)
  # here, ensures that if to_col is not provided, it is from_coo too
  if (missing(to_col))
    to_col   <- enquo(from_col)
  else
    to_col <- enquo(to_col)
  # operate
  x %>% dplyr::mutate(!!to_col := x %>% dplyr::pull(!!from_col) %>% coo_align())
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
#' x <- bot2 %>% pick(1)
#' gg(x)
#'
#' x %>% coo_rotate(pi/2) %>% draw(col="red")
#' x %>% coo_rotate(degrees_to_radians(-45)) %>% draw(col="blue")
#'
#' bot2 %>% coo_rotate(pi) %>% pile()
#'
#' @export
coo_rotate <- function(x, theta = 0, ...) {
  UseMethod("coo_rotate")
}

#' @describeIn coo_rotate default method
#' @export
coo_rotate.default <- function(x, theta = 0, ...) {
  mat <- matrix(c(cos(-theta), sin(-theta), -sin(-theta), cos(-theta)), nrow = 2)
  x %>% as.matrix() %*% mat %>% coo_single()
}

#' @describeIn coo_rotate coo_list method
#' @export
coo_rotate.coo_list <- function(x, theta = 0, ...) {
  mat <- matrix(c(cos(-theta), sin(-theta), -sin(-theta), cos(-theta)), nrow = 2)
  x %>% purrr::map(~.x %>% as.matrix() %*% mat %>% coo_single()) %>% coo_list()
}

#' @describeIn coo_rotate default method
#' @export
coo_rotate.list <- coo_rotate.coo_list

#' @describeIn coo_rotate coo_tbl method
#' @export
coo_rotate.coo_tbl<- function(x, theta = 0, from_col=coo, to_col=coo, ...) {
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
#' x <- bot2 %>% pick(5) %>% coo_center() %>% coo_scale()
#' seq(0, pi, pi/6) %>%
#'   purrr::map(~ x %>% coo_rotatecenter(x, theta=.x, center=c(5, 5)) %>% coo_single) %>%
#'   coo_tbl() %>%
#'   pile()
#' @export
coo_rotatecenter <- function(x, theta=0, center = c(0, 0), ...) {
  UseMethod("coo_rotatecenter")
}

#' @describeIn coo_rotatecenter default method
#' @export
coo_rotatecenter.default <- function(x, theta=0, center = c(0, 0), ...){
  center <- unlist(center) # if passed as data.frame like
  x %>%
    # probably a more direct option
    coo_trans(x_trans = -center[1], y_trans = -center[2]) %>%
    coo_rotate(theta) %>%
    coo_trans(x_trans = center[1], y_trans = center[2])
}

#' @describeIn coo_rotatecenter coo_list method
#' @export
coo_rotatecenter.coo_list <- function(x, theta=0, center = c(0, 0), ...) {
  x %>% purrr::map(coo_rotatecenter, center=center) %>% coo_list()
}

#' @describeIn coo_rotatecenter list method
#' @export
coo_rotatecenter.list <- coo_rotatecenter.coo_list

#' @describeIn coo_rotatecenter coo_tbl method
#' @export
coo_rotatecenter.coo_tbl <- function(x, theta=0, center = c(0, 0), from_col=coo, to_col=coo, ...) {
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
                        coo_rotatecenter(theta=theta, center=center))
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
#'
#' @inherit coo_center params return
#' @param n `integer` desired number of coordinates (required)
#' @family coo_modifyers
#' @family samplers
#' @examples
#' x <- bot2 %>% pick(1) %>% coo_sample(24)
#' x %>% gg()
#' x %>% coo_interpolate(120) %>% gg()
#'
#' bot2$coo %>% coo_sample(12) %>% purrr::map_dbl(nrow)
#' bot2$coo %>% coo_sample(12) %>% coo_interpolate(24) %>% purrr::map_dbl(nrow)
#'
#' bot2 %>% dplyr::slice(1:5) %>% coo_sample(12) -> x
#' pile(x)
#' x %>% coo_interpolate(48) %>% pile()
#'
#' @export
coo_sample <- function(x, n, ...) {
  UseMethod("coo_sample")
}

#' @export
coo_sample.coo_single <- function(x, n, ...) {
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
  x[round(seq(1, nrow(x), len = n + 1)[-(n + 1)]), ]
}

#' @export
coo_sample.list <- function(x, n, ...) {
  x %>% purrr::map(coo_sample, n) %>% coo_list()
}

#' @export
coo_sample.coo_tbl <- function(x, n, from_col=coo, to_col=coo, ...) {
  # tidyeval
  from_col <- enquo(from_col)
  # here, ensures that if to_col is not provided, it is from_coo too
  if (missing(to_col))
    to_col   <- enquo(from_col)
  else
    to_col <- enquo(to_col)
  # operate
  x %>% dplyr::mutate(!!to_col := x %>% dplyr::pull(!!from_col) %>% coo_sample(n=n))
  ########
}

# coo_interpolate -----------------------------------------
#' @describeIn coo_sample Interpolates shape coordinates
#' @export
coo_interpolate <- function(x, n, ...) {
  UseMethod("coo_interpolate")
}

#' @export
coo_interpolate.coo_single <- function(x, n, ...) {
  # early return when unchanged must be returned
  if (nrow(x) == n){
    return(x)
  }
  # interpolate will cut based on perimeter_along
  # with a reference (x) and a target
  # on which we get ideal cutting along, given n
  old_along <- x %>% get_perim_cumsum() %>% unlist()
  new_along <- seq(0, get_perim(x), length = n + 1)[-(n + 1)]

  # we keep the first point and prototype new_x with NAs
  new_x <- dplyr::bind_rows(x[1, ],
                            coo_single(matrix(NA_real_, nrow=n-1, ncol=2)))

  # a loop that will find the embrassing coordinates from original shape
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
coo_interpolate.list <- function(x, n, ...){
  x %>% purrr::map(coo_interpolate, n)
}

#' @export
coo_interpolate.coo_tbl <- function(x, n, from_col=coo, to_col=coo, ...){

  # tidyeval
  from_col <- enquo(from_col)
  # here, ensures that if to_col is not provided, it is from_coo too
  if (missing(to_col))
    to_col   <- enquo(from_col)
  else
    to_col <- enquo(to_col)
  # operate
  x %>% dplyr::mutate(!!to_col := x %>% dplyr::pull(!!from_col) %>% coo_interpolate(n=n))
}

