#' Geometry helpers ------
#'
#'
#' @param seg1,seg2 segment definitions as 2 rows (start, end) x 2 cols
#' @param x1,y1 parameters (x and y diff) of vector 1
#' @param x2,y2 parameters (x and y diff) of vector 2
#' @source Joseph O'Rourke :
#' [compilation of graphics algorithm](http://www.gamers.org/dEngine/rsc/usenet/comp.graphics.algorithms.faq)
#' @name geometry
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

