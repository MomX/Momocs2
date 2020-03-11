# distance helpers ---------



# distance utils ------------

#' Calculates euclidean distance between two points.
#'
#' \code{ed} simply calculates euclidean distance between two points defined by
#' their (x; y) coordinates.
#'
#' @param pt1 (x; y) coordinates of the first point.
#' @param pt2 (x; y) coordinates of the second point.
#' @return Returns the euclidean distance between the two points.
#' @seealso \link{edm}, \link{edm_nearest}, \link{dist}.
#' @examples
#' ed(c(0,1), c(1,0))
#' @export
ed <- function(pt1, pt2) {
  return(sqrt((pt1[1] - pt2[1])^2 + (pt1[2] - pt2[2])^2))
}

#' Calculates euclidean intermediate between two points.
#'
#' \code{edi} simply calculates coordinates of a points at the relative
#' distance \code{r} on the \code{pt1-pt2} defined by their (x; y) coordinates.
#' This function is used internally but may be of interest for other analyses.
#'
#' @param pt1 \eqn{(x; y)} coordinates of the first point.
#' @param pt2 \eqn{(x; y)} coordinates of the second point.
#' @param r the relative distance from \code{pt1} to \code{pt2}.
#' @return returns the \eqn{(x; y)} interpolated coordinates.
#' @seealso \link{ed}, \link{edm}.
#' @examples
#' edi(c(0,1), c(1,0), r = 0.5)
#' @export
edi <- function(pt1, pt2, r = 0.5) {
  return(r * (pt2 - pt1) + pt1)
}

#' Calculates euclidean distance every pairs of points in two matrices.
#'
#' \code{edm} returns the euclidean distances between points \eqn{1 -> n} of
#' two 2-col matrices of the same dimension. This function is used internally
#' but may be of interest for other analyses.
#'
#' If one wishes to align two (or more shapes) Procrustes surimposition may
#' provide a better solution.
#' @param m1 The first \code{matrix} of coordinates.
#' @param m2 The second \code{matrix} of coordinates.
#' @return Returns a \code{vector} of euclidean distances between pairwise
#' coordinates in the two matrices.
#' @seealso \link{ed}, \link{edm_nearest}, \link{dist}.
#' @examples
#' x <- matrix(1:10, nc=2)
#' edm(x, x)
#' edm(x, x+1)
#' @export
edm <- function(m1, m2) {
  return(sqrt(apply((m1 - m2)^2, 1, sum)))
}

#' Calculates the shortest euclidean distance found for every point of one
#' matrix among those of a second.
#'
#' \code{edm_nearest} calculates the shortest euclidean distance found for
#' every point of one matrix among those of a second. In other words, if
#' \code{m1, m2} have \code{n} rows, the result will be the shortest distance
#' for the first point of \code{m1} to any point of \code{m2} and so on,
#' \code{n} times. This function is used internally but may be of interest for
#' other analyses.
#'
#' So far this function is quite time consumming since it performs \eqn{ n
#' \times n } euclidean distance computation.  If one wishes to align two (or
#' more shapes) Procrustes surimposition may provide a better solution.
#' @param m1 The first \code{list} or \code{matrix} of coordinates.
#' @param m2 The second \code{list} or \code{matrix} of coordinates.
#' @param full \code{logical}. Whether to returns a condensed version of the
#' results.
#' @return If \code{full} is \code{TRUE}, returns a \code{list} with two
#' components: \code{d} which is for every point of \code{m1} the shortest
#' distance found between it and any point in \code{m2}, and \code{pos} the
#' (\code{m2}) row indices of these points. Otherwise returns \code{d} as a
#' numeric vector of the shortest distances.
#' @seealso \link{ed}, \link{edm}, \link{dist}.
#' @examples
#' x <- matrix(1:10, nc=2)
#' edm_nearest(x, x+rnorm(10))
#' edm_nearest(x, x+rnorm(10), full=TRUE)
#' @export
edm_nearest <- function(m1, m2, full = FALSE) {
  m1 <- coo_check(m1)
  m2 <- coo_check(m2)
  if (!is.matrix(m1) | !is.matrix(m2))
    stop("Matrices must be provided")
  if (ncol(m1) != 2 | ncol(m2) != 2)
    stop("2-cols matrices must be provided")
  nr <- nrow(m1)
  pos <- d <- numeric(nr)
  for (i in 1:nr) {
    m1.i <- m1[i, ]
    di <- apply(m2, 1, function(x) sqrt(sum((x - m1.i)^2)))
    d[i] <- min(di)
    pos[i] <- which.min(di)
  }
  if (full)
    return(list(d = d, pos = pos)) else return(d)
}


# coo testers ---------------------
# that return logical

# coo_is_closed ----------
#' Test if shapes are closed
#'
#' Returns TRUE/FALSE whether the last coordinate of the shapes is the same
#' as the first one.
#'
#' @aliases coo_is_closed
#' @inheritParams coo_check
#' @return a single or a vector of \code{logical}.
#' @family coo_ utilities
#' @examples
#' coo_is_closed(matrix(1:10, ncol=2))
#' coo_is_closed(coo_close(matrix(1:10, ncol=2)))
#' coo_is_closed(bot)
#' coo_is_closed(coo_close(bot))
#' @export
coo_is_closed <- function(coo) {
  UseMethod("coo_is_closed")
}

#' @export
coo_is_closed.default <- function(coo) {
  coo <- coo_check(coo)
  identical(coo[1, ], coo[nrow(coo), ])
}

#' @export
coo_is_closed.Coo <- function(coo) {
  Coo <- coo
  return(sapply(Coo$coo, coo_is_closed))
}

#' @rdname coo_is_closed
#' @export
is_open <- function(coo) !coo_is_closed(coo)

# is_equallyspacedradii ----------
#' Tests if coordinates likely have equally spaced radii
#'
#' Returns TRUE/FALSE whether the sd of angles between all successive
#' radii is below/above \code{thesh}
#'
#' @inheritParams coo_check
#' @param thres numeric a threshold (arbitrarily \code{pi/90}, eg 2 degrees, by default)
#' @return a single or a vector of \code{logical}. If \code{NA} are returned,
#' some coordinates are likely identical, at least for x or y.
#' @family coo_ utilities
#' @examples
#' bot[1] %>% is_equallyspacedradii
#' bot[1] %>% coo_samplerr(36) %>% is_equallyspacedradii
#' # higher tolerance but wrong
#' bot[1] %>% coo_samplerr(36) %>% is_equallyspacedradii(thres=5*2*pi/360)
#' # coo_interpolate is a better option
#' bot[1] %>% coo_interpolate(1200) %>% coo_samplerr(36) %>% is_equallyspacedradii
#' # Coo method
#' bot %>% coo_interpolate(360) %>% coo_samplerr(36) %>% is_equallyspacedradii
#' @export
is_equallyspacedradii <- function(coo, thres) {
  UseMethod("is_equallyspacedradii")
}

#' @export
is_equallyspacedradii.default <- function(coo, thres=pi/90){
  coo1 <- coo_slide(coo, id = 2)
  cent <- coo_centpos(coo)
  res <- vector("numeric", nrow(coo))
  for (i in 1:nrow(coo)){
    res[i] <- rbind(coo[i, ], cent, coo1[i, ]) %>%
      .coo_angle_edge1("acos") %>% `[`(2)
  }
  sd(res) < thres
}

#' @export
is_equallyspacedradii.Coo <- function(coo, thres=pi/90){
  Coo <- coo
  suppressWarnings(sapply(Coo$coo, is_equallyspacedradii, thres=thres))
}

# # is.likelyopen tries to estimate is a matrix of
# coordinates is likely to be a # closed polygon
# is.likelyclosedpolygon <- function(coo) { x <-
# coo_perimpts(coo) d <- max(x) / median(x[-which.max(x)])
# ifelse(d > 3, TRUE, FALSE)}

# coo_clockwise
# see http://en.wikipedia.org/wiki/Shoelace_formula

#' Tests if shapes are (likely) developping clockwise or anticlockwise
#'
#' @inheritParams coo_check
#' @return a single or a vector of \code{logical}.
#' @family coo_ utilities
#' @examples
#' shapes[4] %>% coo_sample(64) %>% coo_plot()  #clockwise cat
#' shapes[4] %>% coo_likely_clockwise()
#' shapes[4] %>% coo_rev() %>% coo_likely_clockwise()
#'
#' # on Coo
#' shapes %>% coo_likely_clockwise %>% `[`(4)
#' @rdname coo_likely_clockwise
#' @export
coo_likely_clockwise <- function(coo)
  UseMethod("coo_likely_clockwise")

#' @rdname coo_likely_clockwise
#' @export
coo_likely_clockwise.default <- function(coo){
  res <- numeric(nrow(coo)-1)
  for (i in seq_along(res)){
    res[i] <- (coo[i+1, 1] - coo[i, 1]) * (coo[i+1, 2] - coo[i, 2])
  }
  sum(res)>0
}

#' @rdname coo_likely_clockwise
#' @export
coo_likely_clockwise.Coo <- function(coo){
  sapply(coo$coo, coo_likely_clockwise)
}

#' @rdname coo_likely_clockwise
#' @export
coo_likely_anticlockwise <- function(coo){
  !coo_likely_clockwise(coo)
}


# end of coo_utilities
