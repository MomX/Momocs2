# ed ------------------------------------------------------
#' Euclidean distance helpers
#'
#' A little tribe to help with euclidean distances calculations
#'
#' @param x,y [coo_single], matrices or vectors of length 2 (when sensible)
#' @param r `numeric` how much of the distance `d(x -> y)` should we travel?
#'
#' @name euclidean
#' @family geometry
#'
#' @examples
#' x <- c(0, 0)
#' y <- c(1, 1)
#'
#' ed(x, y) # sqrt(2)
#' edi(x, y, 0.25) # c(0.25, 0.25)
#'
#' bot %>% dplyr::slice(1:2) %>% coo_sample(12)  %>% dplyr::pull(coo) -> b
#' ed_pw(b[[1]], b[[2]])
#' # checking
#' purrr::map_dbl(1:12, ~ed(b[[1]][.x, ], b[[2]][.x, ]))
#'
#' bot %>% pick(1) %>% ed_nearest(c(0, 0))
#'
#' # nearest and furthest points
#'
#' set.seed(2329) # for the sake of reproducibility when building the pkg
#' foo <- tibble::tibble(x=runif(5), y=runif(5))
#' z <- c(0.5, 0.5)
#' plot(foo, pch="")
#' text(foo, labels=1:5)
#' points(z[1], z[2], col="red", pch=20)
#'
#' (nearest <- ed_nearest(foo, z))
#' segments(z[1], z[2],
#'          as.numeric(foo[nearest$id, 1]), as.numeric(foo[nearest$id, 2]), col="blue")
#'
#' (furthest <- ed_furthest(foo, z))
#' segments(z[1], z[2],
#'          as.numeric(foo[furthest$id, 1]), as.numeric(foo[furthest$id, 2]), col="red")
#'
#'
#' (calli <- ed_calliper(foo))
#' segments(as.numeric(foo[calli$ids[1], 1]), as.numeric(foo[calli$ids[1], 2]),
#'          as.numeric(foo[calli$ids[2], 1]), as.numeric(foo[calli$ids[2], 2]), col="green")
#'
#'
NULL

#' @describeIn euclidean calculates **e**uclidean **d**istance between two points
#' @export
ed <- function(x, y){
  # to pass coo_single
  x <- unlist(x)
  y <- unlist(y)
  # Pythagoras' theorem
  sqrt((x[1] - y[1])^2 + (x[2] - y[2])^2) %>% unlist()
}

#' @describeIn euclidean calculates **i**ntermediate position between two points
#' @export
edi <- function(x, y, r = 0.5) {
  x + r * (y - x)
}

#' @describeIn euclidean calculates **p**air**w**ise distances between two shapes
#' @export
ed_pw <- function(x, y) {
  sqrt(apply((x - y)^2, 1, sum)) %>% unlist()
}

#' @describeIn euclidean calculates **closest** point to y from x
#' @export
ed_nearest <- function(x, y){
  y <- unlist(y)
  d <- sqrt((x[, 1] - y[1])^2 + (x[, 2] - y[2])^2) %>% unlist()

  list(d   = min(d),
       ids = which.min(d))
}

#' @describeIn euclidean calculates **furthest** point to y from x
#' @export
ed_furthest <- function(x, y){
  y <- unlist(y)
  d <- sqrt((x[, 1] - y[1])^2 + (x[, 2] - y[2])^2) %>% unlist()

  list(d   = max(d),
       ids = which.max(d))
}

#' @describeIn euclidean find **calliper** (max pairwise ed) length
#' @export
ed_calliper <- function(x){
  d <- stats::dist(x, method = "euclidean") %>% as.matrix()
  d[upper.tri(d)] <- 0 # saves a na.rm

  list(d   = max(d),
       ids = which(d == max(d), arr.ind = TRUE) %>% sort())
}

#' @describeIn euclidean find **min**imal **rad**ius (min dist to centroid)
#' @export
ed_minrad <- function(x){
  ed_nearest(x, get_centpos(x))
}

# geometry ------------------------------------------------

#' Geometry helpers ------
#'
#'
#' @param seg1,seg2 segment definitions as 2 rows (start, end) x 2 cols
#' @param x1,y1 parameters (x and y diff) of vector 1
#' @param x2,y2 parameters (x and y diff) of vector 2
#' @source Joseph O'Rourke :
#' [compilation of graphics algorithm](http://www.gamers.org/dEngine/rsc/usenet/comp.graphics.algorithms.faq)
#' @name geometry
#' @family geometry
NULL

#' @describeIn geometry calculate intersection between two segments
#' @export
geometry_intersection_two_segments <- function(seg1, seg2){
  # to follow algo's original syntax
  A <- unlist(seg1[1, ])
  B <- unlist(seg1[2, ])
  C <- unlist(seg2[1, ])
  D <- unlist(seg2[2, ])
  # borrowed from
  # http://www.gamers.org/dEngine/rsc/usenet/comp.graphics.algorithms.faq
  # a fast one! takes 16 microseconds will all my decoration

  # prepare for eqns
  XA <- A[1]
  YA <- A[2]

  XB <- B[1]
  YB <- B[2]

  XC <- C[1]
  YC <- C[2]

  XD <- D[1]
  YD <- D[2]

  # prepare for eqn1 and eqn2
  den   <- ((XB-XA)*(YD-YC)-(YB-YA)*(XD-XC))
  num_r <- ((YA-YC)*(XD-XC)-(XA-XC)*(YD-YC))
  num_s <- ((YA-YC)*(XB-XA)-(XA-XC)*(YB-YA))

  # initialize values
  xy <- lines <- segments <- coincident <- parallel <- FALSE

  # non intersection case
  if (den==0){
    lines <- FALSE
    segment <- FALSE
    if (num_r==0){
      coincident <- TRUE
      parallel   <- TRUE
    } else {
      parallel   <- TRUE
    }
    # intersection case
  } else {

    lines <- TRUE
    segment <- ifelse(((0 <= r  & r <= 1) & (0 <= s & s <= 1)), TRUE, FALSE)

    # now calculate the intersection coordinates
    r <- num_r/den
    xy <- c(x=XA+r*(XB-XA),
            y=YA+r*(YB-YA))
  }

  # return this beauty
  list(lines     = lines,
       segments  = segments,
       concident = coincident,
       parallel  = parallel,
       xy        = xy)
}

#' @describeIn geometry calculate scale and rotation differences between two vectors
#' @export
geometry_diff_two_vectors <- function(x1, y1, x2, y2){
  # ensure numeric
  x1 <- as.numeric(x1)
  y1 <- as.numeric(y1)
  x2 <- as.numeric(x2)
  y2 <- as.numeric(y2)

  # norms
  d1 <- sqrt(sum(x1^2 + y1^2))
  d2 <- sqrt(sum(x2^2 + y2^2))

  # angles
  t1 <- atan2(y1, x1)
  t2 <- atan2(y2, x2)

  # return
  list(scale = d1/d2,
       theta = t2 - t1)
}

#' @describeIn geometry calculate scale and rotation differences between two segments
#' @export
geometry_diff_two_segments <- function(seg1, seg2){
  vec1 <- seg1[2, ] - seg1[1, ]
  vec2 <- seg2[2, ] - seg2[1, ]
  geometry_diff_two_vectors(vec1[1], vec1[2], vec2[1], vec2[2])
}
