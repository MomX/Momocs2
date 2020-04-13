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
  .msg_info("coo_center: not defined on this class")
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
#' Returns a shape translate by `x` and `y`.
#'
#' @inherit coo_center params return
#' @param x_trans,y_trans `numeric` how much translate on each direction
#' @family coo_modifyers
#' @family translations
#' @examples
#' bot$coo[[1]] %>% coo_trans
#'
#' @export
coo_trans <- function(x, x_trans=0, y_trans=0, from_col, to_col, ...) {
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
coo_trans.coo_list <- function(x, x_trans=0, y_trans=0, ...){
  x %>% purrr::map(coo_trans, x_trans=x_trans, y_trans=y_trans) %>% coo_list()
}

#' @describeIn coo_trans mom_tbl method
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
#' bot$coo[[1]] %>% coo_scale
#'
#' @export
coo_scale <- function(x, scale, from_col, to_col, ...) {
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
coo_scale.coo_list <- function(x, scale, ...){
  x <- purrr::map(x, coo_single)
  if (missing(scale))
    scale <- purrr::map_dbl(x, get_centsize)
  purrr::map2(x, scale, coo_scale) %>% coo_list()
}

#' @describeIn coo_scale mom_tbl method
#' @export
coo_scale.mom_tbl <- function(x, scale, from_col=coo, to_col={{from_col}}, ...) {
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_scale())
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

#' @describeIn coo_template default method
#' @export
coo_template.default <- function(x, size=1, ...) {
  # get the rescaling ratio
  k <- min(size/get_diffrange(x))
  # center and apply it
  x %>%
    coo_center %>%
    dplyr::mutate(x=.data$x*k, y=.data$y*k)
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
#' bot$coo[[5]] %>% coo_align
#' @export
coo_align <- function(x, from_col=coo, to_col=coo, ...) {
  UseMethod("coo_align")
}

#' @describeIn coo_align default method
#' @export
coo_align.default <- function(x, ...){
  (as.matrix(x) %*% (svd(stats::var(as.matrix(x)))$u)) %>% coo_single()
}

#' @describeIn coo_align list method
#' @export
coo_align.coo_list <- function(x, ...){
  x %>% purrr::map(coo_align) %>% coo_list()
}

#' @describeIn coo_align mom_tbl method
#' @export
coo_align.mom_tbl <- function(x, from_col=coo, to_col={{from_col}}, ...){
  # tidyeval
  c(from_col, to_col) %<-% tidyeval_coo_modifyers(from_col={{from_col}}, to_col={{to_col}})

  # operate
  x %>% dplyr::mutate(!!to_col := x %>%
                        dplyr::pull(!!from_col) %>%
                        coo_align())
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

#' @describeIn coo_rotatecenter mom_tbl method
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
  .msg_info("coo_sample_prop: not defined on this class")
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
.msg_info("coo_sample_rr: not defined on this class")
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
  .msg_info("coo_smooth: not defined on this class")
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
.msg_info("coo_smooth_curve: not defined on this class")
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

# CLOSING -------------------------------------------------

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
  .msg_info("coo_close: not defined on this class")
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

#' @describeIn coo_close Unclose shapes
#' @export
coo_unclose <- function(x, from_col, to_col, ...) {
  UseMethod("coo_unclose")
}

#' @export
coo_unclose.default <- function(x, ...){
  .msg_info("coo_unclose: not defined on this class")
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

# coo_up and friends --------------------------------------

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
.msg_info("coo_up: not defined on this class")
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

#' @describeIn coo_up filter lower part of a shape
#' @export
coo_down <- function(x, from_col, to_col, ...) {
  UseMethod("coo_down")
}

#' @export
coo_down.default <- function(x, ...){
.msg_info("coo_down: not defined on this class")
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

#' @describeIn coo_up filter left part of a shape
#' @export
coo_left <- function(x, from_col, to_col, ...) {
  UseMethod("coo_left")
}

#' @export
coo_left.default <- function(x, ...){
.msg_info("coo_left: not defined on this class")
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

#' @describeIn coo_up filter right part of a shape
#' @export
coo_right <- function(x, from_col, to_col, ...) {
  UseMethod("coo_right")
}

#' @export
coo_right.default <- function(x, ...){
.msg_info("coo_right: not defined on this class")
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

