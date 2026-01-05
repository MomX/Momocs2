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
#' @param axes Logical. Whether to draw axes.
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
p <- function(x, xlim, ylim, axes=TRUE){
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
  if (axes){
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
  }
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
#' @param .ldk_col Character. Name of landmark column. If `NULL`, uses `colname_ldk`.
#'
#' @return Invisibly returns `x` (for piping).
#'
#' @details
#' Stacks all shapes on a single plot. Works with:
#' - Single matrix: plots that shape
#' - List of matrices: plots all shapes overlaid
#' - Tibble: plots all shapes from coo column (or specified `.cols`)
#'
#' If a tibble has a landmark column (e.g., `coo_ldk`), landmarks will be drawn
#' in addition to outlines. Otherwise, falls back to heuristic: if shapes have
#' fewer than 32 points, draws as landmarks; otherwise draws as outlines.
#'
#' @examples
#' pile(shapes$cat)
#' pile(shapes)
#' pile(bot)
#' pile(bot, .cols = "coo")
#'
#' @keywords internal
#' @export
pile <- function(x, ..., .cols = NULL, .ldk_col = NULL) {
  original_x <- x

  # Handle tibble dispatch with landmark awareness
  if (is.data.frame(x)) {
    # Get coo column(s) with tidyeval
    .cols_quo <- rlang::enquo(.cols)
    if (rlang::quo_is_null(.cols_quo)) {
      cols_to_use <- get_coo_cols(x, NULL)
    } else {
      cols_idx <- tidyselect::eval_select(.cols_quo, x)
      cols_to_use <- names(cols_idx)
    }

    # Only work with single coo column
    if (length(cols_to_use) > 1) {
      warning("Multiple coo columns found. Using first: '", cols_to_use[1],
              "'. Specify .cols to choose a different one.")
      cols_to_use <- cols_to_use[1]
    }

    # Check for landmark column
    .ldk_col_quo <- rlang::enquo(.ldk_col)
    ldk_col_name <- if (!rlang::quo_is_null(.ldk_col_quo)) {
      rlang::as_name(.ldk_col_quo)
    } else {
      paste0(cols_to_use, "_ldk")
    }

    # Extract coo list
    coo_list <- x[[cols_to_use]]

    # If landmarks exist, draw outlines + landmarks
    if (ldk_col_name %in% names(x)) {
      # Extract landmark coordinates
      ldk_coords_list <- get_ldk(x, .cols = !!rlang::sym(cols_to_use),
                                 .ldk_col = !!rlang::sym(ldk_col_name))[[paste0(cols_to_use, "_ldk_coords")]]

      # Draw
      p(coo_list, ...)
      draw_outlines(coo_list, ...)
      draw_first_point(coo_list, ...)
      draw_landmarks(ldk_coords_list, col = "red", pch = 1, ...)

      return(invisible(original_x))
    }

    # No landmarks - use coo list for heuristic
    x <- coo_list
  }

  # Convert single matrix to list for consistency
  if (is.matrix(x)) {
    x <- list(x)
  }

  # Heuristic when no landmark info available
  if (length(x) == 1) {
    n <- nrow(x[[1]])
  } else {
    n <- min(sapply(x, nrow))
  }

  if (n < 32) {
    p(x, ...)
    draw_landmarks(x, ...)
    draw_centroid(x, ...)
  } else {
    p(x, ...)
    draw_outlines(x, ...)
    draw_first_point(x, ...)
  }

  invisible(original_x)
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
#' @param relative Logical. If TRUE (default), uses `coo_template_relatively()` to
#'   preserve relative sizes. If FALSE, uses `coo_template()` to scale all shapes
#'   to the same size.
#' @param .label Column name for labels to display at each shape's origin. Only works
#'   with tibbles. Can be specified with or without quotes.
#' @param label_cex Numeric. Character expansion for labels. Default is 0.5.
#' @param ... Additional arguments (reserved for future use).
#' @param .cols Column name(s) to process when `x` is a tibble. If `NULL`,
#'   automatically detects columns containing coo objects.
#' @param .ldk_col Character. Name of landmark column. If `NULL`, uses `colname_ldk`.
#'
#' @return Invisibly returns list of translated matrices (for piping).
#'
#' @details
#' Arranges shapes in a grid mosaic. By default, shapes are templated relatively
#' to preserve their size relationships - perfect for family pictures!
#'
#' Works with:
#' - Single matrix: plots that shape
#' - List of matrices: arranges all shapes in grid
#' - Tibble: arranges all shapes from coo column (or specified `.cols`)
#'
#' Grid dimensions can be controlled via `nrow`, `ncol`, or `ratio`. If none
#' are specified, creates a square-ish layout.
#'
#' If a tibble has a landmark column (e.g., `coo_ldk`), landmarks will be drawn
#' in addition to outlines. Otherwise, falls back to heuristic: if shapes have
#' fewer than 32 points, draws as landmarks; otherwise draws as outlines.
#'
#' @examples
#' mosaic(shapes$cat)
#' mosaic(shapes)
#' mosaic(bot)
#' mosaic(shapes, nrow = 2)
#' mosaic(shapes, ratio = 16/9)
#'
#' # Preserve relative sizes (default)
#' shapes %>% mosaic(relative = TRUE)
#'
#' # All same size
#' shapes %>% mosaic(relative = FALSE)
#'
#' # With labels from tibble column
#' bot %>% mosaic(.label = type)
#' bot %>% mosaic(.label = "type", label_cex = 0.75)
#'
#' @keywords internal
#' @export
mosaic <- function(x, ratio = NULL, nrow = NULL, ncol = NULL,
                   relative = TRUE, .label = NULL, label_cex = 0.5,
                   ..., .cols = NULL, .ldk_col = NULL) {
  original_x <- x
  has_landmarks <- FALSE
  ldk_list <- NULL
  labels <- NULL

  # Handle tibble dispatch with landmark awareness
  if (is.data.frame(x)) {
    # Get coo column(s) with tidyeval
    .cols_quo <- rlang::enquo(.cols)
    if (rlang::quo_is_null(.cols_quo)) {
      cols_to_use <- get_coo_cols(x, NULL)
    } else {
      cols_idx <- tidyselect::eval_select(.cols_quo, x)
      cols_to_use <- names(cols_idx)
    }

    # Only work with single coo column
    if (length(cols_to_use) > 1) {
      warning("Multiple coo columns found. Using first: '", cols_to_use[1],
              "'. Specify .cols to choose a different one.")
      cols_to_use <- cols_to_use[1]
    }

    # Check for landmark column
    .ldk_col_quo <- rlang::enquo(.ldk_col)
    ldk_col_name <- if (!rlang::quo_is_null(.ldk_col_quo)) {
      rlang::as_name(.ldk_col_quo)
    } else {
      paste0(cols_to_use, "_ldk")
    }

    # Check for label column
    .label_quo <- rlang::enquo(.label)
    if (!rlang::quo_is_null(.label_quo)) {
      label_col_idx <- tidyselect::eval_select(.label_quo, original_x)
      label_col_name <- names(label_col_idx)[1]
      labels <- as.character(original_x[[label_col_name]])
    }

    # Extract coo list
    x <- original_x[[cols_to_use]]

    # If landmarks exist, extract them
    if (ldk_col_name %in% names(original_x)) {
      has_landmarks <- TRUE
      ldk_coords_df <- get_ldk(original_x, .cols = !!rlang::sym(cols_to_use),
                               .ldk_col = !!rlang::sym(ldk_col_name))
      ldk_list <- ldk_coords_df[[paste0(cols_to_use, "_ldk_coords")]]
    }
  }

  # Convert single matrix to list
  if (is.matrix(x)) {
    x <- list(x)
  }

  # Template shapes (relative or absolute)
  if (relative) {
    coo_list <- coo_template_relatively(x, size = 0.95)
  } else {
    coo_list <- coo_template(x, size = 0.95)
  }

  # Template landmarks if they exist
  if (has_landmarks && !is.null(ldk_list)) {
    if (relative) {
      ldk_list <- coo_template_relatively(ldk_list, size = 0.95)
    } else {
      ldk_list <- coo_template(ldk_list, size = 0.95)
    }
  }

  n <- length(coo_list)

  # Handle single shape case
  if (n == 1) {
    # Just plot centered at origin
    if (has_landmarks && !is.null(ldk_list[[1]])) {
      p(coo_list, axes = FALSE)
      draw_outlines(coo_list, ...)
      draw_landmarks(ldk_list, col = "red", pch = 1, ...)
    } else {
      min_n <- nrow(coo_list[[1]])
      if (min_n < 32) {
        p(coo_list, axes = FALSE)
        draw_landmarks(coo_list, ...)
        draw_centroid(coo_list, ...)
      } else {
        p(coo_list, axes = FALSE)
        draw_outlines(coo_list, ...)
        draw_first_point(coo_list, ...)
      }
    }

    # Add label if provided
    if (!is.null(labels)) {
      text(0, 0, labels = labels[1], cex = label_cex, col = "black")
    }

    return(invisible(coo_list))
  }

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
  pos <- pos[nrow:1, ]  # flip rows so shape 1 is top-left

  # now translate each shape to its grid position
  coo_translated <- vector("list", n)
  ldk_translated <- if (has_landmarks) vector("list", n) else NULL

  for (i in seq_len(n)) {
    # find position in grid
    idx <- which(pos == i, arr.ind = TRUE)
    trans_x <- idx[2] - 0.5  # Column (x-offset)
    trans_y <- idx[1] - 0.5  # Row (y-offset)

    # translate coo
    coo_translated[[i]] <- coo_list[[i]]
    coo_translated[[i]][, 1] <- coo_list[[i]][, 1] + trans_x
    coo_translated[[i]][, 2] <- coo_list[[i]][, 2] + trans_y

    # translate landmarks if they exist
    if (has_landmarks && !is.null(ldk_list[[i]]) && nrow(ldk_list[[i]]) > 0) {
      ldk_translated[[i]] <- ldk_list[[i]]
      ldk_translated[[i]][, 1] <- ldk_list[[i]][, 1] + trans_x
      ldk_translated[[i]][, 2] <- ldk_list[[i]][, 2] + trans_y
    }
  }

  # Draw with landmarks if available
  if (has_landmarks) {
    p(coo_translated, axes = FALSE)
    draw_outlines(coo_translated, ...)
    draw_landmarks(ldk_translated, col = "red", pch = 3, ...)
  } else {
    # Heuristic when no landmark info available
    min_n <- min(sapply(coo_translated, nrow))
    if (min_n < 32) {
      p(coo_translated, axes = FALSE)
      draw_landmarks(coo_translated, ...)
      draw_centroid(coo_translated, ...)
    } else {
      p(coo_translated, axes = FALSE)
      draw_outlines(coo_translated, ...)
      draw_first_point(coo_translated, ...)
    }
  }

  # Add labels if provided
  if (!is.null(labels)) {
    for (i in seq_len(n)) {
      idx <- which(pos == i, arr.ind = TRUE)
      trans_x <- idx[2] - 0.5
      trans_y <- idx[1] - 0.5
      text(trans_x, trans_y, labels = labels[i], cex = label_cex, col = "black")
    }
  }

  # and return this beauty
  invisible(coo_translated)
}
