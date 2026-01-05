# base_plotter --------
#' Minimal plotting system for morphometric shapes
#'
#' A collection of functions for creating clean, publication-ready visualizations
#' of morphometric data. The \code{p()} function initializes a minimal plot, and
#' \code{draw_*} functions add layers. All functions return their input invisibly,
#' enabling pipe-friendly workflows.
#'
#' @param x A matrix (single shape) or list of matrices containing (x, y) coordinates
#' @param xlim Numeric vector of length 2 giving x-axis limits. If missing, calculated from data
#' @param ylim Numeric vector of length 2 giving y-axis limits. If missing, calculated from data
#' @param col Character. Color for points, lines, or axes
#' @param pch Integer. Point character type
#' @param cex Numeric. Character/point expansion factor
#' @param lwd Numeric. Line width
#' @param label Character. Label to display at first point (default: "v")
#' @param ... Additional graphical parameters passed to underlying functions
#'
#' @return All functions invisibly return \code{x} as a list (for piping)
#'
#' @details
#' \strong{p():} The base plotter function that opens a device, sets up minimal
#' axes with 1:1 aspect ratio, and prepares the plot area.
#'
#' \strong{draw_landmarks():} Adds landmark points to the current plot.
#'
#' \strong{draw_outlines():} Draws outline curves by connecting coordinates with lines.
#'
#' \strong{draw_centroid():} Marks the centroid (mean x, mean y) of each shape.
#'
#' \strong{draw_first_point():} Marks the first point with a rotated label that
#' points from the first toward the second point.
#'
#' @examples
#' # Basic plotting
#' shapes[1] %>% p() %>% draw_outlines()
#' shapes %>% p() %>% draw_outlines()
#'
#' # Chaining draw functions
#' p(shapes) |> draw_outlines() |> draw_centroid()
#'
#' # With custom styling
#' p(shapes) |>
#'   draw_outlines(col = "blue", lwd = 0.5) |>
#'   draw_centroid(col = "red", cex = 0.5)
#'
#' @rdname p
#' @export
p <- function(x, xlim, ylim){
  # cosmetics
  axes_col = "grey50"
  tick_length = 1/4
  # open device if none exists
  if (dev.cur() == 1) dev.new()
  # if x is a single shape, turn it into a list
  if (!is.list(x) | is.data.frame(x))
    x <- list(x)
  # calculate full window
  w <- do.call("rbind", x)
  # and if missing, assign
  if (missing(xlim)) xlim <- range(w[, 1])
  if (missing(ylim)) ylim <- range(w[, 2])
  # define nice and homogeneous margins
  par(mar = c(2, 2, 0.5, 0.5))
  # empty but initialized plot
  plot(0, 0, pch=3, col="grey50", cex=0.5,
       xlim = xlim, ylim = ylim, asp = 1,
       xlab = "", ylab = "", axes = FALSE, frame.plot = FALSE)
  # minimal axes with only 3 ticks each (min, mid, max)
  x_ticks <- pretty(c(xlim[1],  xlim[2]), n=2, min.n=2)
  y_ticks <- pretty(c(ylim[1],  ylim[2]), n=2, min.n=2)
  # x-axis
  axis(1, at = x_ticks, labels = signif(x_ticks, 2),
       tcl = tick_length, cex.axis = 0.7, padj = -2, lwd = 0,
       col = axes_col, col.axis = axes_col, col.ticks = axes_col, lwd.ticks = 0.5)
  # y-axis
  axis(2, at = y_ticks, labels = signif(y_ticks, 2), las=1, hadj = 0.25, padj = 0.5,
       tcl = tick_length, cex.axis = 0.7,  lwd = 0,
       col = axes_col, col.axis = axes_col, col.ticks = axes_col, lwd.ticks = 0.5)
  # pass x
  invisible(x)
}

#' @rdname p
#' @export
draw_landmarks <- function(x, col="grey20", pch=3, cex=0.25, ...){
  for (i in seq_along(x))
    points(x[[i]], col=col, pch=pch, cex=cex, ...)
  invisible(x)
}

#' @rdname p
#' @export
draw_landmarks_as_numbers <- function(x, col="grey20", pch=3, cex=0.5, ...){
  for (i in seq_along(x))
    text(x[[i]], labels=1:nrow(x[[i]]), col=col, pch=pch, cex=cex, ...)
  invisible(x)
}

#' @rdname p
#' @export
draw_outlines <- function(x, col="grey20", lwd=0.2, ...){
  for (i in seq_along(x))
    lines(x[[i]], col=col, lwd=lwd, ...)
  invisible(x)
}

#' @rdname p
#' @export
draw_centroid <- function(x, col="orange", cex=1/4, pch=3, ...){
  for (i in seq_along(x))
    points(mean(x[[i]][, 1]), mean(x[[i]][, 2]), col=col, pch=pch, cex=cex,  ...)
  invisible(x)
}

#' @rdname p
#' @export
draw_first_point <- function(x, col="grey20", cex=1/3, label="v", ...) {
  for (i in seq_along(x)){
    # first two points
    p1 <- x[[i]][1, ]
    p2 <- x[[i]][2, ]
    # calculate angle from p1 to p2 (in degrees)
    theta <- atan2(p2[2] - p1[2], p2[1] - p1[1]) * 180 / pi
    # add 90 degrees because label is usually "v" and points down
    rotation <- theta + 90
    text(p1[1], p1[2], label=label, col=col, cex=cex, ...)
  }
  # pass
  invisible(x)
}

#' @rdname p
#' @param links A two-column matrix where each row defines a segment.
#'   First column is the start landmark index, second column is the end landmark index.
#'   This matrix is recycled across all shapes in \code{x}.
#' @examples
#' # dummy links, mosquito people please have mercy!
# links <- matrix(c(16, 18,
#                   15, 16,
#                   15, 3,
#                   15, 4,
#                   18, 17,
#                   17, 8,
#                   17, 9), ncol=2, byrow=TRUE)
# wings$coo %>% p %>% draw_landmarks_as_numbers(col="blue") %>% draw_segments(links)
#'
#' @export
draw_links <- function(x, links, col="grey20", lwd=0.5, ...){
  # validate links
  if (!is.matrix(links) || ncol(links) != 2) {
    stop("links must be a two-column matrix")
  }

  for (i in seq_along(x)){
    for (j in seq_len(nrow(links))){
      # extract the two landmark indices
      idx1 <- links[j, 1]
      idx2 <- links[j, 2]

      # check validity
      if (idx1 > nrow(x[[i]]) || idx2 > nrow(x[[i]])) {
        warning("Link indices exceed number of landmarks in shape ", i)
        next
      }

      # draw segment
      segments(x[[i]][idx1, 1], x[[i]][idx1, 2],
               x[[i]][idx2, 1], x[[i]][idx2, 2],
               col=col, lwd=lwd, ...)
    }
  }
  invisible(x)
}

# pile ----
#' Plot shapes stacked together
#'
#' Display one or more shapes stacked on the same plot.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ... Additional arguments passed to plotting functions.
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return Invisibly returns `x` (for piping).
#'
#' @details
#' Stacks all shapes on a single plot. Works with:
#' - Single matrix: plots that shape
#' - List of matrices: plots all shapes overlaid
#' - Tibble: plots all coo columns (or specified `.cols`)
#'
#' @examples
#' pile(shapes$cat)
#' pile(shapes)
#' pile(bot)
#' pile(bot, .cols = "coo")
#'
#' @keywords internal
#' @export
pile <- function(x, ..., .cols = NULL) {

  # Handle dispatch
  if (is.data.frame(x)) {
    if (is.null(.cols)) {
      cols_to_use <- get_coo_cols(x, NULL)
    } else {
      .cols_quo <- rlang::enquo(.cols)
      cols_idx <- tidyselect::eval_select(.cols_quo, x)
      cols_to_use <- names(cols_idx)
    }
    x <- lapply(x[, cols_to_use, drop = FALSE], identity)
    # Flatten if single column
    if (length(x) == 1) {
      x <- x[[1]]
    }
  }

  # simple logic to determine whether to draw landmarks or outlines
  # single shape case
  if (is.matrix(x))
    n <- nrow(x)
  else
    n <- sapply(x, nrow) %>% min()

  if (n < 32){
    x %>% p() %>% draw_landmarks() %>% draw_centroid()
  } else {
    x %>% p() %>% draw_outlines() %>% draw_first_point()
  }

  invisible(x)
}


# mosaic ----
#' Plot shapes in grid mosaic layout
#'
#' Display one or more shapes arranged in a grid layout with automatic positioning.
#'
#' @param x A matrix (nx2), list of matrices, or tibble with coo columns.
#' @param ratio Numeric. Target aspect ratio (width/height) for the mosaic.
#'   If NULL and nrow/ncol not specified, creates square-ish layout.
#' @param nrow Integer. Number of rows in the grid.
#' @param ncol Integer. Number of columns in the grid.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#'
#' @return Invisibly returns list of translated matrices (for piping).
#'
#' @details
#' Arranges shapes in a grid mosaic. Shapes are assumed to be templated
#' (centered in a 1x1 bounding box). Works with:
#' - Single matrix: plots that shape
#' - List of matrices: arranges all shapes in grid
#' - Tibble: arranges all shapes from coo columns (or specified `.cols`)
#'
#' Grid dimensions can be controlled via `nrow`, `ncol`, or `ratio`. If none
#' are specified, creates a square-ish layout.
#'
#' @examples
#' mosaic(shapes$cat)
#' mosaic(shapes)
#' mosaic(bot)
#' mosaic(shapes, nrow = 2)
#' mosaic(shapes, ratio = 16/9)
#'
#' @keywords internal
#' @export
mosaic <- function(x, ratio = NULL, nrow = NULL, ncol = NULL, ..., .cols = NULL) {
  # Handle dispatch - convert to list if needed
  if (is.data.frame(x)) {
    if (is.null(.cols)) {
      cols_to_use <- get_coo_cols(x, NULL)
    } else {
      .cols_quo <- rlang::enquo(.cols)
      cols_idx <- tidyselect::eval_select(.cols_quo, x)
      cols_to_use <- names(cols_idx)
    }
    x <- lapply(x[, cols_to_use, drop = FALSE], identity)
    if (length(x) == 1) {
      x <- x[[1]]
    }
  }

  # Convert single matrix to list
  if (is.matrix(x)) {
    x <- list(x)
  }

  # Now x is a list of matrices
  coo_list <- x
  n <- length(coo_list)

  # grid dimensions and ratio-ing
  if (is.null(nrow) && is.null(ncol)) {
    if (is.null(ratio)) {
      # default to squarish layout
      ncol <- ceiling(sqrt(n))
      nrow <- ceiling(n / ncol)
    } else {
      # use ratio to optimize layout (width/height -> ncol/nrow)
      nrow <- ceiling(sqrt(n / ratio))
      ncol <- ceiling(n / nrow)
    }
  } else if (is.null(nrow)) {
    nrow <- ceiling(n / ncol)
  } else if (is.null(ncol)) {
    ncol <- ceiling(n / nrow)
  }

  # sanity check
  if (nrow * ncol < n) {
    stop("nrow * ncol must be >= length(coo_list)")
  }

  # create position matrix
  pos <- matrix(1:(nrow * ncol), nrow, ncol, byrow = TRUE)
  # flip rows so shape 1 is top-left.
  # drop handles single shape cases
  pos <- pos[nrow:1, , drop=FALSE]

  # now translate each shape to its grid position
  coo_translated <- vector("list", n)
  for (i in seq_len(n)) {
    # find position in grid
    idx <- which(pos == i, arr.ind = TRUE)
    trans_x <- idx[2] - 0.5  # Column (x-offset)
    trans_y <- idx[1] - 0.5  # Row (y-offset)
    # translate
    coo_translated[[i]] <- coo_list[[i]]
    coo_translated[[i]][, 1] <- coo_list[[i]][, 1] + trans_x
    coo_translated[[i]][, 2] <- coo_list[[i]][, 2] + trans_y
  }

  # simple logic to determine whether to draw landmarks or outlines
  min_n <- sapply(coo_translated, nrow) %>% min()
  if (min_n < 32){
    coo_translated %>% p() %>% draw_landmarks() %>% draw_centroid()
  } else {
    coo_translated %>% p() %>% draw_outlines() %>% draw_first_point()
  }

  # and return this beauty
  invisible(coo_translated)
}

