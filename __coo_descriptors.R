# coo_lw ------
#' Calculates length and width of a shape
#'
#' Returns the length and width of a shape based on their iniertia axis
#' i.e. alignment to the x-axis. The length is defined as
#' the range along the x-axis; the width as the range on the y-axis.
#' @param coo a \code{matrix} of (x; y) coordinates or Coo object
#' @return a vector of two \code{numeric}: the length and the width.
#' @seealso \link{coo_length}, \link{coo_width}.
#' @family coo_ descriptors
#' @examples
#' coo_lw(bot[1])
#' @export
coo_lw <- function(coo){
  UseMethod("coo_lw")
}

#' @export
coo_lw.default <- function(coo) {
  coo <- coo_check(coo)
  d <- apply(coo_align(coo), 2, range)
  return(abs(d[2, ] - d[1, ]))
}

#' @export
coo_lw.Coo <- function(coo){
  sapply(coo$coo, coo_lw)
}

#' Calculates the length of a shape
#'
#' Nothing more than \code{coo_lw(coo)[1]}.
#' @param coo a \code{matrix} of (x; y) coordinates or a Coo object
#' @return the length (in pixels) of the shape
#' @seealso \link{coo_lw}, \link{coo_width}
#' @details This function can be used to integrate size - if meaningful -
#' to Coo objects. See also \link{coo_centsize} and \link{rescale}.
#' @family coo_ descriptors
#' @examples
#' coo_length(bot[1])
#' coo_length(bot)
#' mutate(bot, size=coo_length(bot))
#' @export
coo_length <- function(coo){
  UseMethod("coo_length")
}
#' @export
coo_length.default <- function(coo) {
  return(coo_lw(coo)[1])
}
#' @export
coo_length.Coo <- function(coo){
  sapply(coo$coo, coo_length)
}

#' Calculates the width of a shape
#'
#' Nothing more than \code{coo_lw(coo)[2]}.
#' @param coo a \code{matrix} of (x; y) coordinates or Coo object
#' @return the width (in pixels) of the shape
#' @seealso \link{coo_lw}, \link{coo_length}.
#' @family coo_ descriptors
#' @examples
#' coo_width(bot[1])
#' @export
coo_width <- function(coo) {
  UseMethod("coo_width")
}

#' @export
coo_width.default <- function(coo){
  return(coo_lw(coo)[2])
}

#' @export
coo_width.Coo <- function(coo){
  sapply(coo$coo, coo_width)
}

# coo_boundingbox -----------
#' Calculates coordinates of the bounding box
#' @inheritParams coo_check
#' @return `data.frame` with coordinates of the bounding box
#' @examples
#' bot[1] %>% coo_boundingbox()
#' bot %>% coo_boundingbox()
#' @family coo_ utilities
#' @family coo_ descriptors
#' @export
coo_boundingbox <- function(coo){
  UseMethod("coo_boundingbox")
}

#' @export
coo_boundingbox.default <- function(coo){
  coo %>% apply(2, range) %>% as.numeric() %>%
    sapply(list) %>% `names<-`(c("x0", "x1", "y0", "y1")) %>%
    dplyr::as_data_frame()
}

#' @export
coo_boundingbox.Coo <- function(coo){
  lapply(coo$coo, coo_boundingbox) %>%
    do.call("rbind", .)
}




# coo_area ------
#' Calculates the area of a shape
#'
#' Calculates the area for a (non-crossing) shape.
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return \code{numeric}, the area.
#' @note Using \code{area.poly} in gpc package is a good idea, but their licence
#' impedes Momocs to rely on it. but here is the function to do it, once gpc is loaded:
#' \code{ area.poly(as(coo, 'gpc.poly')) }
#' @family coo_ descriptors
#' @examples
#' coo_area(bot[1])
#' # for the distribution of the area of the bottles dataset
#' hist(sapply(bot$coo, coo_area), breaks=10)
#' @export
coo_area <- function(coo){
  UseMethod("coo_area")
}
#' @export
coo_area.default <- function(coo) {
  coo <- coo_check(coo)
  coo <- coo_close(coo)
  nr <- nrow(coo) - 1
  y <- x <- numeric(nr)
  for (i in 1:nr) {
    x[i] <- coo[i, 1] * coo[i + 1, 2]
    y[i] <- coo[i + 1, 1] * coo[i, 2]
  }
  area <- (0.5 * (sum(x) - sum(y)))
  return(abs(area))
}

#' @export
coo_area.Coo <- function(coo){
  sapply(coo$coo, coo_area)
}
# area.poly(as(coo, 'gpc.poly'))}


# coo_rectilinearity ------
#' Calculates the rectilinearity of a shape
#'
#' As proposed by Zunic and Rosin (see below). May need some testing/review.
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`
#' @return `numeric` for a single shape, `list` for `Coo`
#' @note due to the laborious nature of the algorithm (in nb.pts^2),
#' and of its implementation, it may be very long to compute.
#' @source Zunic J, Rosin PL. 2003. Rectilinearity measurements for polygons.
#' IEEE Transactions on Pattern Analysis and Machine Intelligence 25: 1193-1200.
#' @family coo_ descriptors
#' @examples
#' bot[1] %>%
#'     coo_sample(32) %>% # for speed sake only
#'     coo_rectilinearity
#'
#' bot %>%
#'     slice(1:3) %>% coo_sample(32) %>% # for speed sake only
#'     coo_rectilinearity
#' @export
coo_rectilinearity <- function(coo) {
  UseMethod("coo_rectilinearity")
}

#' @export
coo_rectilinearity.default <- function(coo) {
  # some check
  coo <- coo_check(coo)
  if (coo_is_closed(coo)) {
    coo_c <- coo
    coo <- coo_unclose(coo)
  } else {
    coo_c <- coo_close(coo)
  }
  # we deduce it for the algo
  n <- nrow(coo)
  k <- 4 * n
  # here starts the computation as given by Zunic and Rosin we
  # calculate l1 and l2 for every edge
  l1 <- function(x1, y1, x2, y2) {
    abs(x1 - x2) + abs(y1 - y2)
  }
  l2 <- function(x1, y1, x2, y2) {
    sqrt((x1 - x2)^2 + (y1 - y2)^2)
  }
  # l2 is redefined here for coherence with the paper, but is
  # equivalent to coo_perimpts(coo)
  l2.e <- l1.e <- numeric(n)
  for (i in 1:n) {
    x1 <- coo_c[i, 1]
    y1 <- coo_c[i, 2]
    x2 <- coo_c[i + 1, 1]
    y2 <- coo_c[i + 1, 2]
    l1.e[i] <- l1(x1, y1, x2, y2)
    l2.e[i] <- l2(x1, y1, x2, y2)
  }  # sum(l2.e) == coo_perim(coo)
  # 'step 1' as in Zunic and Rosin
  theta <- coo_angle_edges(coo)
  theta.k <- abs(c(theta - pi/2, theta - pi, theta - 3 * pi/2,
                   theta - 2 * pi))
  alpha.k <- sort(theta.k)
  # 'step 2' as in Zunic and Rosin
  P1.Pa <- numeric(k)
  for (j in 1:k) {
    P1.Pa_n <- numeric(n)
    for (i in 1:n) {
      cos.ij <- cos(theta[i] + alpha.k[j])
      sin.ij <- sin(theta[i] + alpha.k[j])
      a.ij <- ifelse(cos.ij > 0, l2.e[i], -l2.e[i])
      b.ij <- ifelse(sin.ij > 0, l2.e[i], -l2.e[i])
      P1.Pa_n[i] <- a.ij * cos.ij + b.ij * sin.ij
    }
    P1.Pa[j] <- sum(P1.Pa_n)
  }
  # 'step 3' as in Zunic and Rosin
  return((4/(4 - pi)) * ((sum(l2.e)/min(P1.Pa)) - (pi/4)))
}

#' @export
coo_rectilinearity.Coo <- function(coo) {
  lapply(coo$coo, coo_rectilinearity)
}

# coo_circularity ------
#' Calculates the Haralick's circularity of a shape
#'
#' `coo_circularity` calculates the 'circularity measure'. Also called 'compactness'
#' and 'shape factor' sometimes. `coo_circularityharalick` calculates Haralick's circularity which is less sensible
#' to digitalization noise than `coo_circularity`.
#' `coo_circularitynorm` calculates 'circularity', also called compactness
#' and shape factor, but normalized to the unit circle.
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`
#' @return `numeric` for single shapes, `list` for `Coo` of
#' the corresponding circularity measurement.
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#'
#' # coo_circularity
#' bot[1] %>% coo_circularity()
#' bot %>%
#'     slice(1:5) %>% # for speed sake only
#'     coo_circularity
#'
#' # coo_circularityharalick
#' bot[1] %>% coo_circularityharalick()
#' bot %>%
#'     slice(1:5) %>% # for speed sake only
#'     coo_circularityharalick
#'
#' # coo_circularitynorm
#' bot[1] %>% coo_circularitynorm()
#' bot %>%
#'     slice(1:5) %>% # for speed sake only
#'     coo_circularitynorm
#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularity <- function(coo){
  UseMethod("coo_circularity")
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularity.default <- function(coo) {
  return(coo_perim(coo)^2/coo_area(coo))
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularity.Coo <- function(coo) {
  lapply(coo$coo, coo_circularity)
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularityharalick <- function(coo) {
  UseMethod("coo_circularityharalick")
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularityharalick.default <- function(coo) {
  cd <- coo_centdist(coo)
  return(mean(cd)/sd(cd))
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularityharalick.Coo <- function(coo) {
  lapply(coo$coo, coo_circularityharalick)
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularitynorm <- function(coo){
  UseMethod("coo_circularitynorm")
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularitynorm.default <- function(coo) {
  return(coo_perim(coo)^2/(coo_area(coo) * 4 * pi))
}

#' @rdname coo_circularity
#' @name coo_circularity
#' @export
coo_circularitynorm.Coo <- function(coo) {
  lapply(coo$coo, coo_circularitynorm)
}

# coo_eccentricity ------
#' Calculates the eccentricity of a shape
#'
#'
#' `coo_eccentricityeigen` uses the ratio of
#' the eigen values (inertia axes of coordinates).
#' `coo_eccentricityboundingbox` uses the width/length ratio (see [coo_lw]).
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`
#' @return `numeric` for single shapes, `list` for `Coo`.
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @seealso \link{coo_eccentricityboundingbox}
#' @family coo_ descriptors
#' @examples
#' # coo_eccentricityeigen
#' bot[1] %>% coo_eccentricityeigen()
#' bot %>%
#'     slice(1:3) %>% # for speed sake only
#'     coo_eccentricityeigen()
#'
#' # coo_eccentricityboundingbox
#' bot[1] %>% coo_eccentricityboundingbox()
#' bot %>%
#'     slice(1:3) %>% # for speed sake only
#'     coo_eccentricityboundingbox()
#' @rdname coo_eccentricity
#' @name coo_eccentricity
#' @export
coo_eccentricityeigen <- function(coo) {
  UseMethod("coo_eccentricityeigen")
}

#' @rdname coo_eccentricity
#' @name coo_eccentricity
#' @export
coo_eccentricityeigen.default <- function(coo) {
  coo <- coo_check(coo)
  eig <- eigen(cov(coo))$values
  return(eig[2]/eig[1])
}

#' @rdname coo_eccentricity
#' @name coo_eccentricity
#' @export
coo_eccentricityeigen.Coo <- function(coo) {
  lapply(coo$coo, coo_eccentricityeigen)
}

#' @rdname coo_eccentricity
#' @name coo_eccentricity
#' @export
coo_eccentricityboundingbox <- function(coo) {
  UseMethod("coo_eccentricityboundingbox")
}

#' @rdname coo_eccentricity
#' @name coo_eccentricity
#' @export
coo_eccentricityboundingbox.default <- function(coo) {
  coo <- coo_check(coo)
  lw <- coo_lw(coo)
  return(lw[2]/lw[1])
}

#' @rdname coo_eccentricity
#' @name coo_eccentricity
#' @export
coo_eccentricityboundingbox.Coo <- function(coo) {
  lapply(coo$coo, coo_eccentricityboundingbox)
}

#' Calculates the elongation of a shape
#'
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return numeric, the eccentricity of the bounding box
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#' coo_elongation(bot[1])
#' # on Coo
#' # for speed sake
#' bot %>% slice(1:3) %>% coo_elongation
#' @export
coo_elongation <- function(coo) {
  UseMethod("coo_elongation")
}

#' @export
coo_elongation.default <- function(coo) {
  coo <- coo_check(coo)
  lw <- coo_lw(coo)
  return(1 - lw[2]/lw[1])
}

#' @export
coo_elongation.Coo <- function(coo) {
  sapply(coo$coo, coo_elongation)
}

# coo_rectangularity -----

#' Calculates the rectangularity of a shape
#'
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`
#' @return `numeric` for a single shape, `list` for `Coo`
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#' coo_rectangularity(bot[1])
#'
#' bot %>%
#'     slice(1:3) %>% # for speed sake only
#'     coo_rectangularity
#' @export
coo_rectangularity <- function(coo) {
  UseMethod("coo_rectangularity")
}

#' @export
coo_rectangularity.default <- function(coo) {
  coo <- coo_check(coo)
  abr <- prod(coo_lw(coo))
  return(coo_area(coo)/abr)
}

#' @export
coo_rectangularity.Coo <- function(coo) {
  lapply(coo$coo, coo_rectangularity)
}



# coo_convexity -------
#' Calculates the convexity of a shape
#'
#' Calculated using a ratio of the eigen values (inertia axis)
#' @param coo a \code{matrix} of (x; y) coordinates.
#' @return `numeric` for a single shape, `list` for a `Coo`
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#' coo_convexity(bot[1])
#' bot %>%
#'     slice(1:3) %>% # for speed sake only
#'     coo_convexity()
#' @export
coo_convexity <- function(coo) {
  UseMethod("coo_convexity")
}

#' @export
coo_convexity.default <- function(coo) {
  coo <- coo_check(coo)
  return(coo_perim(coo_chull(coo))/coo_perim(coo))
}

#' @export
coo_convexity.Coo <- function(coo){
  lapply(coo$coo, coo_convexity)
}
# coo_solidity -------
#' Calculates the solidity of a shape
#'
#' Calculated using the ratio of the shape area and the convex hull area.
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`
#' @return `numeric` for a single shape, `list` for `Coo`
#' @source Rosin PL. 2005. Computing global shape measures.
#' Handbook of Pattern Recognition and Computer Vision. 177-196.
#' @family coo_ descriptors
#' @examples
#' coo_solidity(bot[1])
#'
#' bot %>%
#'     slice(1:3) %>%  # for speed sake only
#'     coo_solidity
#' @export
coo_solidity <- function(coo){
  UseMethod("coo_solidity")
}

#' @export
coo_solidity.default <- function(coo) {
  coo <- coo_check(coo)
  return(coo_area(coo)/coo_area(coo_chull(coo)))
}

#' @export
coo_solidity.Coo <- function(coo) {
  lapply(coo$coo, coo_solidity)
}

# coo_tac -------
#' Calculates the total absolute curvature of a shape
#'
#' Calculated using the sum of the absolute value of the second derivative of
#' the \code{smooth.spline} prediction for each defined point.
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`
#' @return `numeric` for a single shape and for `Coo`
#' @source Siobhan Braybrook.
#'
#' @family coo_ descriptors
#' @examples
#' coo_tac(bot[1])
#'
#' bot %>%
#'     slice(1:3) %>%  # for speed sake only
#'     coo_tac
#' @export
coo_tac <- function(coo){
  UseMethod("coo_tac")
}

#' @export
coo_tac.default <- function(coo) {
  coo <- coo_check(coo)
  tac <- sum(abs(predict(stats::smooth.spline(coo), deriv = 2)$y))
  return(tac)
}

#' @export
coo_tac.Coo <- function(coo) {
  sapply(coo$coo, coo_tac)
}


# 3. coo shape descriptors
# coo_centpos --------------
#' Calculate centroid coordinates
#'
#' Returns the (x; y) centroid coordinates of a shape.
#' @inheritParams coo_check
#' @return (x; y) coordinates of the centroid as a vector or a matrix.
#' @examples
#' b <- bot[1]
#' coo_plot(b)
#' xy <- coo_centpos(b)
#' points(xy[1], xy[2], cex=2, col='blue')
#' # on a Coo
#' coo_centpos(bot)
#' @family centroid functions
#' @family coo_ utilities
#' @export
coo_centpos <- function(coo) {
  UseMethod("coo_centpos")
}

#' @export
coo_centpos.default <- function(coo) {
  coo <- coo_check(coo)
  return(apply(coo, 2, mean))
}

#' @export
coo_centpos.Coo <- function(coo) {
  Coo <- coo
  centpos <- t(sapply(Coo$coo, coo_centpos))
  colnames(centpos) <- c("x", "y")  # pure cosmetics
  return(centpos)
}

# coo_centsize --------------
#' Calculates centroid size
#' @inheritParams coo_check
#' @return \code{numeric}, the centroid size.
#' @details This function can be used to integrate size - if meaningful -
#' to Coo objects. See also \link{coo_length} and \link{rescale}.
#' @examples
#' coo_centsize(bot[1])
#' # on a Coo
#' coo_centsize(bot)
#' # add it to $fac
#' mutate(bot, size=coo_centsize(bot))
#' @family centroid functions
#' @family coo_utilities
#' @export
coo_centsize <- function(coo){
  UseMethod("coo_centsize")
}

#' @export
coo_centsize.default <- function(coo) {
  coo <- coo_check(coo)
  cp <- coo_centpos(coo)
  mean(sqrt((coo[, 1] - cp[1])^2 + (coo[, 2] - cp[2])^2))
}

#' @export
coo_centsize.Coo <- function(coo){
  sapply(coo$coo, coo_centsize)
}



# coo_calliper --------------
#' Calculates the calliper length
#'
#' Also called the Feret's diameter, the longest distance between two points of
#' the shape provided.
#' @aliases coo_calliper
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`
#' @param arr.ind \code{logical}, see below.
#' @return \code{numeric}, the centroid size. If \code{arr.ind=TRUE}, a `data_frame`.
#' @examples
#' b <- bot[1]
#' coo_calliper(b)
#' p <- coo_calliper(b, arr.ind=TRUE)
#' p
#' p$length
#' ids <- p$arr_ind[[1]]
#' coo_plot(b)
#' segments(b[ids[1], 1], b[ids[1], 2], b[ids[2], 1], b[ids[2], 2], lty=2)
#'
#' # on a Coo
#' bot %>%
#' coo_sample(32) %>% # for speed sake
#' coo_calliper()
#'
#' bot %>%
#' coo_sample(32) %>% # for speed sake
#' coo_calliper(arr.ind=TRUE)
#'
#' @family calliper functions
#' @family coo_ utilities
#' @export
coo_calliper <- function(coo, arr.ind=FALSE){
  UseMethod("coo_calliper")
}

#' @export
coo_calliper.default <- function(coo, arr.ind = FALSE) {
  coo <- coo_check(coo)
  d <- dist(coo, method = "euclidean")
  # we check if there is no ex aequo
  ea <- length(which(d == max(d), arr.ind = TRUE))
  if (length(ea) > 1) {
    message("coo_length: at least two lengths are ex aequo")
  }
  if (arr.ind) {
    arr.ind <- which(as.matrix(d) == max(d), arr.ind = TRUE)
    # to return a vector (numeric and sorted) of the rows between
    # which the max length has been found
    arr.ind <- sort(as.numeric(arr.ind[1, ]))
    return(dplyr::data_frame(length = max(d), arr_ind = list(arr.ind)))
  } else {
    return(max(d))
  }
}

#' @export
coo_calliper.Coo <- function(coo, arr.ind = FALSE) {
  if (arr.ind)
    lapply(coo$coo, coo_calliper, arr.ind=arr.ind) %>%
    do.call("rbind", .)
  else
    lapply(coo$coo, coo_calliper, arr.ind=arr.ind)
}


# coo_nb ----------------
#' Counts coordinates
#'
#' Returns the number of coordinates, for a single shape or a Coo object
#' @inheritParams coo_check
#' @return either a single numeric or a vector of numeric
#' @family coo_ utilities
#' @examples
#' # single shape
#' coo_nb(bot[1])
#' # Coo object
#' coo_nb(bot)
#' @export
coo_nb <- function(coo){
  UseMethod("coo_nb")
}

#' @export
coo_nb.default <- function(coo){
  nrow(coo)
}

#' @export
coo_nb.Coo <- function(coo){
  sapply(coo$coo, nrow) %>% unlist()
}



#' Calculates all scalar descriptors of shape
#'
#' See examples for the full list.
#'
#' @details [coo_rectilinearity] being not particularly optimized, it takes around 30 times more
#' time to include it than to calculate _all_ others and is thus not includedby default.
#' by default.
#' @param coo a \code{matrix} of (x; y) coordinates or any `Coo`
#' @param rectilinearity `logical` whether to include rectilinearity using [coo_rectilinearity]
#' @return `data_frame`
#'
#' @family coo_ descriptors
#' @examples
#'
#' df <- bot %>% coo_scalars() # pass bot %>% coo_scalars(TRUE) if you want rectilinearity
#' colnames(df) %>% cat(sep="\n") # all scalars used
#'
#' # a PCA on all these descriptors
#' TraCoe(coo_scalars(bot), fac=bot$fac) %>% PCA %>% plot_PCA(~type)
#'
#' @export
coo_scalars <- function(coo, rectilinearity=FALSE){
  UseMethod("coo_scalars")
}

#' @export
coo_scalars.default <- function(coo, rectilinearity=FALSE){
  res <- dplyr::data_frame(
    area=coo_area(coo),
    calliper=coo_calliper(coo),
    centsize=coo_centsize(coo),
    circularity=coo_circularity(coo),
    circularityharalick=coo_circularityharalick(coo),
    circularitynorm=coo_circularitynorm(coo),
    convexity=coo_convexity(coo),
    eccentricityboundingbox=coo_eccentricityboundingbox(coo),
    eccentricityeigen=coo_eccentricityeigen(coo),
    elongation=coo_elongation(coo),
    length=coo_length(coo),
    perim=coo_perim(coo),
    rectangularity=coo_rectangularity(coo),
    solidity=coo_solidity(coo),
    width=coo_width(coo)
  )
  if (rectilinearity)
    res$rectilinearity <- coo_rectilinearity(coo)
  res
}

#' @export
coo_scalars.Coo <- function(coo, rectilinearity=FALSE){
  lapply(coo$coo, function(.x) .x %>% coo_scalars(rectilinearity=rectilinearity)) %>%
    dplyr::bind_rows()
}






