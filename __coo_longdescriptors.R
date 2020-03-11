
# coo_range ----------------
#' Calculate coordinates range
#'
#' `coo_range` simply returns the range,
#' `coo_range_enlarge` enlarges it by a `k` proportion.
#' `coo_diffrange` return the amplitude (ie diff after `coo_range`)
#'
#' @inheritParams  coo_check
#' @param k `numeric` proportion by which to enlarge it
#'
#' @return a matrix of range such as `(min, max) x (x, y)`
#' @family coo_ utilities
#' @name coo_range
#' @rdname coo_range
#' @examples
#' bot[1] %>% coo_range # single shape
#' bot    %>% coo_range # Coo object
#'
#' bot[1] %>% coo_range_enlarge(1/50) # single shape
#' bot    %>% coo_range_enlarge(1/50) # Coo object
#' @export
coo_range <- function(coo){
  UseMethod("coo_range")
}

#' @rdname coo_range
#' @export
coo_range.default <- function(coo){
  res <- apply(coo, 2, range)
  dimnames(res) <- list(c("min", "max"), c("x", "y"))
  res
}

#' @rdname coo_range
#' @export
coo_range.Coo <- function(coo){
  lapply(coo$coo, coo_range) %>%
    do.call("rbind", .) %>%
    coo_range
}


#' @rdname coo_range
#' @export
coo_range_enlarge <- function(coo, k){
  UseMethod("coo_range_enlarge")
}

#' @rdname coo_range
#' @export
coo_range_enlarge.default <- function(coo, k=0){
  m <- coo_range(coo)
  g <- apply(m, 2, diff)*k
  m[1, ] <- m[1, ] - g
  m[2, ] <- m[2, ] + g
  m
}

#' @rdname coo_range
#' @export
coo_range_enlarge.Coo <- coo_range_enlarge.default

#' @rdname coo_range
#' @export
coo_range_enlarge.list <- function(coo, k=0){
  m <- lapply(coo, coo_range) %>%
    do.call("rbind", .) %>%
    coo_range
  g <- apply(m, 2, diff)*k
  m[1, ] <- m[1, ] - g
  m[2, ] <- m[2, ] + g
  m
}

#' @rdname coo_range
#' @export
coo_diffrange <- function(coo){
  UseMethod("coo_diffrange")
}

#' @rdname coo_range
#' @export
coo_diffrange.default <- function(coo){
  res <- apply(coo, 2, function(x) diff(range(x)))
  names(res) <- c("x", "y")
  res
}

#' @rdname coo_range
#' @export
coo_diffrange.Coo <- function(coo){
  lapply(x$coo, coo_diffrange) %>%
    do.call("rbind", .)
}


#' @rdname coo_range
#' @export
coo_diffrange.list <- function(coo){
  lapply(coo, coo_diffrange) %>%
    do.call("rbind", .)
}


# coo_angle ------

#' Calculates the angle of every edge of a shape
#'
#' Returns the angle (in radians) of every edge of a shape,
# either signed ('atan2') or not ('acos').
#' @param coo a \code{matrix} or a list of (x; y) coordinates or any `Coo`
#' @param method 'atan2' (or 'acos') for a signed (or not) angle.
#' @return \code{numeric} the angles in radians for every edge.
#' @note \code{coo_thetapts} is deprecated and will be removed
#' in future releases.
#' @family coo_ descriptors
#' @examples
#' b <- coo_sample(bot[1], 64)
#' coo_angle_edges(b)
#' @rdname coo_angle_edges
#' @export
coo_angle_edges <- function(coo, method = c("atan2", "acos")[1]){
  UseMethod("coo_angle_edges")
}

.coo_angle_edge1 <- function(coo, method = c("atan2", "acos")[1]) {
  .check(is.matrix(coo) && nrow(coo)==3 && ncol(coo)==2,
         "coo must be a 3x2 matrix")
  a <- apply(coo[2:1, ], 2, diff)
  b <- apply(coo[2:3, ], 2, diff)
  if (method == "atan2") {
    ang <- atan2(a[1] * b[2] - a[2] * b[1],
                 a[1] * b[1] + a[2] * b[2])
  }
  if (method == "acos") {
    ang <- acos(sum(a * b) /
                  (sqrt(sum(a * a)) * sqrt(sum(b * b))))
  }
  ang
}

#' @rdname coo_angle_edges
#' @export
coo_angle_edges.default <- function(coo, method = c("atan2", "acos")[1]) {
  coo <- coo_check(coo)
  coo <- coo_close(coo)
  coo <- rbind(coo[nrow(coo) - 1, ], coo)
  theta <- numeric()
  for (i in 1:(nrow(coo) - 2)) {
    theta[i] <- .coo_angle_edge1(coo[i:(i + 2), ], method = method)
  }
  return(theta)
}

#' @rdname coo_angle_edges
#' @export
coo_angle_edges.Coo <- function(coo, method = c("atan2", "acos")[1]) {
  lapply(coo$coo, coo_angle_edges, method=method)
}

#' Calculates the tangent angle along the perimeter of a
#' shape
#'
#' Calculated using complex numbers and returned in radians
#' minus the first one (modulo 2*pi).
#' @param coo a matrix of coordinates or any `Coo`
#' @return `numeric`, the tangent angle along the perimeter, or a
#' `list` of those for `Coo`
#' @seealso \link{tfourier}
#' @family coo_ descriptors
#' @examples
#' b <- bot[1]
#' phi  <- coo_angle_tangent(b)
#' phi2 <- coo_angle_tangent(coo_smooth(b, 2))
#' plot(phi, type='l')
#' plot(phi2, type='l', col='red') # ta is very sensible to noise
#'
#' # on Coo
#' bot %>% coo_angle_tangent
#' @rdname coo_angle_tangent
#' @export
coo_angle_tangent <- function(coo) {
  UseMethod("coo_angle_tangent")
}

#' @rdname coo_angle_tangent
#' @export
coo_angle_tangent.default <- function(coo) {
  p <- nrow(coo)
  tangvect <- coo - rbind(coo[p, ], coo[-p, ])
  tet1 <- Arg(complex(real = tangvect[, 1], imaginary = tangvect[,
                                                                 2]))
  tet0 <- tet1[1]
  t1 <- seq(0, 2 * pi, length = (p + 1))[1:p]
  phi <- (tet1 - tet0 - t1)%%(2 * pi)
  return(phi)
}

#' @rdname coo_angle_tangent
#' @export
coo_angle_tangent.Coo <- function(coo) {
  lapply(coo$coo, coo_angle_tangent)
}

#' @rdname coo_angle_tangent
#' @export
coo_tangle <- function(coo){
  .Deprecated("coo_angle_tangent")
}


# coo_chull --------
#' Calculates the (recursive) convex hull of a shape
#'
#' `coo_chull` returns the ids of points that define the convex hull of a shape. A simple wrapper
#' around \link{chull}, mainly used in graphical functions.
#'
#' `coo_chull_onion` recursively find their convex hull,
#' remove them, until less than 3 points are left.
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`.
#' @param close `logical` whether to close onion rings (`TRUE` by default)
#' @return `coo_chull` returns a `matrix` of points defining
#' the convex hull of the shape; a `list` for `Coo`.
#' `coo_chull_onion` returns a `list` of successive onions rings,
#'  and a `list` of `list`s for `Coo`.
#' @family coo_ descriptors
#' @examples
#' # coo_chull
#' h <- coo_sample(hearts[4], 32)
#' coo_plot(h)
#' ch <- coo_chull(h)
#' lines(ch, col='red', lty=2)
#'
#' bot %>% coo_chull
#'
#' coo_chull_onion
#' x <- bot %>% efourier(6) %>% PCA
#' all_whisky_points <- x %>% as_df() %>% filter(type=="whisky") %>% select(PC1, PC2)
#' plot(x, ~type, eig=FALSE)
#' peeling_the_whisky_onion <- all_whisky_points %>% as.matrix %>% coo_chull_onion()
#' # you may need to par(xpd=NA) to ensure all segments
#' # even those outside the graphical window are drawn
#' peeling_the_whisky_onion$coo %>% lapply(coo_draw)
#' # simulated data
#' xy <- replicate(2, rnorm(50))
#' coo_plot(xy, poly=FALSE)
#' xy %>% coo_chull_onion() %$% coo %>%
#' lapply(polygon, col="#00000022")
#' @rdname coo_chull
#' @name coo_chull
#' @export
coo_chull <- function(coo){
  UseMethod("coo_chull")
}

#' @rdname coo_chull
#' @name coo_chull
#' @export
coo_chull.default <- function(coo) {
  coo <- coo_check(coo)
  return(coo[grDevices::chull(coo), ])
}

#' @rdname coo_chull
#' @name coo_chull
#' @export
coo_chull.Coo <- function(coo) {
  lapply(coo$coo, coo_chull)
}


# coo_chull_onion -----------
#' @rdname coo_chull
#' @name coo_chull
#' @export
coo_chull_onion <- function(coo, close=TRUE){
  UseMethod("coo_chull_onion")
}

#' @rdname coo_chull
#' @name coo_chull
#' @export
coo_chull_onion.default <- function(coo, close=TRUE){
  coo %<>% as.matrix()
  res <- list()
  i <- 1
  while(is.matrix(coo) && nrow(coo) > 3){
    chi_ids <- grDevices::chull(coo[, 1], coo[, 2])
    # if asked to close, then close ids and coos will follow
    if(close)
      chi_ids <- c(chi_ids, chi_ids[1])

    res$ids[[i]] <- chi_ids
    res$coo[[i]] <- coo[chi_ids, ]
    coo <- coo[-chi_ids, ]
    i <- i + 1
  }
  res
}

#' @rdname coo_chull
#' @name coo_chull
#' @export
coo_chull_onion.Coo <- function(coo, close=TRUE){
  lapply(coo$coo, coo_chull_onion)
}




# coo_centdist --------------
#' Returns the distance between everypoints and the centroid
#'
#' For every point of the shape, returns the (centroid-points) distance.
#' @aliases coo_centdist
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return a \code{matrix} of (x; y) coordinates.
#' @examples
#' b <- coo_sample(bot[1], 64)
#' d <- coo_centdist(b)
#' barplot(d, xlab="Points along the outline", ylab="Distance to the centroid (pixels)")
#' @family centroid functions
#' @family coo_ utilities
#' @export
coo_centdist <- function(coo){
  UseMethod("coo_centdist")
}
#' @export
coo_centdist.default <- function(coo) {
  coo <- coo_check(coo)
  return(apply(coo, 1, function(x) ed(coo_centpos(coo), x)))
}

#' @export
coo_centdist.Coo <- function(coo){
  lapply(coo$coo, coo_centdist)
}

# coo_perimpts --------------
#' Calculates perimeter and variations
#'
#' `coo_perim` calculates the perimeter;
#' `coo_perimpts` calculates the euclidean distance between every points of a shape;
#'`coo_perimcum` does the same and calculates and cumulative sum.
#' @param coo \code{matrix} of (x; y) coordinates or any `Coo`
#' @return \code{numeric} the distance between every point or
#' a `list` of those.
#' @examples
#' # for speed sake
#' b1 <- coo_sample(bot[1], 12)
#' b5 <- bot %>% slice(1:5) %>% coo_sample(12)
#'
#' # coo_perim
#' coo_perim(b1)
#' coo_perim(b5)
#'
#' # coo_perimpts
#' coo_perimpts(b1)
#' b5 %>% coo_perimpts()
#'
#' # coo_perimcum
#' b1 %>% coo_perimcum()
#' b5 %>% coo_perimcum()
#' @family perimeter functions
#' @family coo_ utilities
#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perimpts <- function(coo) {
  UseMethod("coo_perimpts")
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perimpts.default <- function(coo) {
  coo <- coo_check(coo)
  n <- nrow(coo)
  d <- sqrt(apply((coo - coo_slide(coo, n))^2, 1, sum))[-1]
  return(d)
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perimpts.Coo <- function(coo) {
  lapply(coo$coo, coo_perimpts)
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perimcum <- function(coo) {
  UseMethod("coo_perimcum")
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perimcum.default <- function(coo) {
  coo <- coo_check(coo)
  d <- cumsum(sqrt(apply((coo - rbind(coo[1, ], coo[-(dim(coo)[1]),
                                                    ]))^2, 1, sum)))
  return(d)
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perimcum.Coo <- function(coo) {
  lapply(coo$coo, coo_perimcum)
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perim <- function(coo) {
  UseMethod("coo_perim")
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perim.default <- function(coo) {
  return(sum(coo_perimpts(coo)))
}

#' @rdname coo_perim
#' @name coo_perim
#' @export
coo_perim.Coo <- function(coo) {
  sapply(coo$coo, coo_perim)
}



# coo_dxy -------------------
#' Calculate abscissa and ordinate on a shape
#'
#' A simple wrapper to calculate dxi - dx1 and dyi - dx1.
#' @param coo a matrix (or a list) of (x; y) coordinates or any `Coo`
#' @return a `data.frame` with two components \code{dx} and \code{dy} for single shapes
#' or a `list` of such `data.frame`s for `Coo`
#' @family exemplifying functions
#' @family coo_ utilities
#' @examples
#' coo_dxy(coo_sample(bot[1], 12))
#'
#' bot %>%
#'     slice(1:5) %>% coo_sample(12) %>%  # for readability and speed only
#'     coo_dxy()
#' @export
coo_dxy <- function(coo) {
  UseMethod("coo_dxy")
}

#' @export
coo_dxy.default <- function(coo) {
  coo <- coo_check(coo)
  dx <- coo[, 1] - coo[1, 1]
  dy <- coo[, 2] - coo[1, 2]
  return(dplyr::data_frame(dx = dx, dy = dy))
}

#' @export
coo_dxy.Coo <- function(coo) {
  lapply(coo$coo, coo_dxy)
}
