# Combined calibration functions for Momocs2
# This file contains all calibration functions for opoly, npoly, dct, and eft
# Plot methods are defined once at the end with @rdname plot_calibrate

# npoly_calibrate_r2 ----

#' Calibrate polynomial degree using R2
#'
#' Calculate R2 (goodness of fit) across a range of polynomial degrees to
#' determine optimal degree for natural polynomial fitting.
#'
#' @param x A tibble with coo columns containing open curves.
#' @param degree_range Integer vector. Range of degrees to test. Default is 2:12.
#' @param id Integer vector or NULL. Which shapes to use for calibration.
#'   If NULL, uses all shapes (or samples if many). If integer vector, uses
#'   those specific row indices.
#' @param thresh Numeric vector. R2 thresholds for recommendations.
#'   Default is c(0.90, 0.95, 0.99).
#' @param .cols Column name(s) to process. If NULL, uses first coo column.
#'   Supports tidyeval (e.g., `.cols = VD`).
#'
#' @return A list with class `c("calibrate_r2", "calibrate")` containing:
#' * `data`: Tibble with columns shape, degree, and r2
#' * `recommended`: Named integer vector of minimum degree per threshold
#' * `summary`: Character string with human-readable recommendation
#'
#' The list has attributes:
#' * `method`: "npoly"
#' * `metric`: "r2"
#' * `param_name`: "degree"
#' * `thresh`: the threshold values used
#'
#' @details
#' This function fits natural polynomials at each degree in `degree_range`
#' for the specified shapes and calculates R2 (proportion of variance explained).
#' Higher degrees always produce higher R2 but may overfit. The goal is to find
#' the minimum degree that achieves acceptable R2.
#'
#' ## Choosing thresholds
#'
#' Common interpretations:
#' * 0.90 (90%): Rough approximation, may miss details
#' * 0.95 (95%): Good balance for most applications
#' * 0.99 (99%): High fidelity, captures fine details
#'
#' ## Sampling strategy
#'
#' When `id = NULL`:
#' * If <= 30 shapes: uses all
#' * If > 30 shapes: randomly samples 30 (set seed for reproducibility)
#'
#' ## Plotting
#'
#' Use `plot(cal)` to visualize results as a boxplot with threshold lines.
#'
#' @examples
#' # Calibrate on subset of olea
#' data(olea)
#' cal <- olea[1:10, ] %>%
#'   npoly_calibrate_r2(.cols=VD)
#'
#' # Examine results
#' cal$recommended
#' cal$summary
#' plot(cal)
#'
#' # Use recommended degree
#' olea %>% npoly(degree = cal$recommended["0.99"])
#'
#' # Tidyeval column selection
#' olea[1:10, ] %>%
#'   npoly_calibrate_r2(.cols = VD)
#'
#' @seealso [npoly_calibrate_reconstruction()], [npoly()]
#'
#' @export
npoly_calibrate_r2 <- function(x,
                               degree_range = 2:12,
                               id = NULL,
                               thresh = c(0.90, 0.95, 0.99),
                               .cols = NULL) {

  # Input validation
  if (!is.data.frame(x)) {
    stop("x must be a tibble/data.frame with coo columns")
  }

  # Get coo column to work with (tidyeval support)
  .cols_quo <- rlang::enquo(.cols)
  if (rlang::quo_is_null(.cols_quo)) {
    .cols <- get_coo_cols(x, NULL)
    message(sprintf("Using coo column: '%s'", .cols))
  } else {
    cols_idx <- tidyselect::eval_select(.cols_quo, x)
    .cols <- names(cols_idx)[1]
    message(sprintf("Using coo column: '%s'", .cols))
  }

  # Determine which shapes to use
  n_shapes <- nrow(x)
  if (is.null(id)) {
    if (n_shapes <= 30) {
      id <- 1:n_shapes
      message(sprintf("Using all %d shapes", n_shapes))
    } else {
      id <- sample(n_shapes, 30)
      message(sprintf("Randomly sampled 30 shapes from %d total", n_shapes))
    }
  } else {
    # Validate id
    if (any(id > n_shapes) || any(id < 1)) {
      stop(sprintf("id values must be between 1 and %d", n_shapes))
    }
    message(sprintf("Using %d specified shapes", length(id)))
  }

  # Prepare result matrix
  n_degrees <- length(degree_range)
  n_id <- length(id)
  res <- matrix(NA, nrow = n_id, ncol = n_degrees)
  rownames(res) <- id
  colnames(res) <- paste0("degree", degree_range)

  # Calculate R2 for each shape and degree
  message(sprintf("Calculating R2 for degrees %d to %d...",
                  min(degree_range), max(degree_range)))

  for (i in seq_along(id)) {
    shape_idx <- id[i]
    coo <- x[[.cols]][[shape_idx]]

    for (j in seq_along(degree_range)) {
      deg <- degree_range[j]
      # Use raw = TRUE to get R2
      fit <- .npoly(coo, degree = deg, raw = TRUE)
      res[i, j] <- fit$r2
    }
  }

  # Convert to tibble for modern handling
  data_tib <- as.data.frame(res)
  data_tib$shape <- rownames(res)
  data_tib <- tidyr::pivot_longer(data_tib,
                                  cols = -shape,
                                  names_to = "degree",
                                  values_to = "r2")
  data_tib$degree <- as.integer(sub("degree", "", data_tib$degree))
  data_tib <- dplyr::arrange(data_tib, shape, degree)

  # Calculate median R2 per degree
  med_r2 <- apply(res, 2, median)

  # Find minimum degree for each threshold
  recommended <- integer(length(thresh))
  names(recommended) <- as.character(thresh)

  for (i in seq_along(thresh)) {
    meets_thresh <- which(med_r2 >= thresh[i])
    if (length(meets_thresh) == 0) {
      recommended[i] <- NA
      warning(sprintf("No degree achieves R2 >= %.3f", thresh[i]))
    } else {
      recommended[i] <- degree_range[min(meets_thresh)]
    }
  }

  # Generate summary text
  summary_lines <- character(length(thresh))
  for (i in seq_along(thresh)) {
    if (!is.na(recommended[i])) {
      summary_lines[i] <- sprintf("Use degree >= %d for %.0f%% R2",
                                  recommended[i], thresh[i] * 100)
    } else {
      summary_lines[i] <- sprintf("No degree achieves %.0f%% R2 in tested range",
                                  thresh[i] * 100)
    }
  }
  summary_text <- paste(summary_lines, collapse = "\n")

  # Create result object
  result <- list(
    data = data_tib,
    recommended = recommended,
    summary = summary_text
  )

  # Add attributes for plotting
  attr(result, "method") <- "npoly"
  attr(result, "metric") <- "r2"
  attr(result, "param_name") <- "degree"
  attr(result, "value_name") <- "r2"
  attr(result, "thresh") <- thresh

  # Set class for S3 dispatch
  class(result) <- c("calibrate_r2", "calibrate", "list")

  # Print summary
  cat("\n")
  cat(summary_text, "\n")
  cat("\nUse plot(result) to visualize\n")

  invisible(result)
}


# npoly_calibrate_reconstruction ----

#' Calibrate polynomial degree using visual reconstruction
#'
#' Visually compare reconstructed shapes at different polynomial degrees to
#' assess fit quality and determine optimal degree.
#'
#' @param x A tibble with coo columns containing open curves.
#' @param degree_range Integer vector. Range of degrees to display. Default is 2:10 (9 panels).
#' @param id Integer or NULL. Which shape to display. If NULL, randomly samples one.
#' @param nb_pts Integer. Number of points for reconstruction. Default is 120.
#' @param .cols Column name(s) to process. If NULL, uses first coo column.
#'   Supports tidyeval (e.g., `.cols = VD`).
#'
#' @return A list with class `c("calibrate_reconstruction", "calibrate")` containing:
#' * `data`: List of reconstructed coordinates at each degree
#' * `original`: Original shape coordinates (not scaled)
#' * `degree_range`: The degrees displayed
#' * `id`: Index of shape used
#'
#' The list has attributes:
#' * `method`: "npoly"
#' * `metric`: "reconstruction"
#'
#' @details
#' This function creates a multi-panel plot (3x3 grid for default 9 degrees) showing:
#' * Original shape in black
#' * Reconstructed shape in red
#' * One panel per degree in `degree_range`
#' * Visual assessment of how reconstruction improves with degree
#'
#' This is the most intuitive calibration method - you can directly see when
#' adding more degrees stops meaningfully improving the fit.
#'
#' @examples
#' # Calibrate on single shape from olea (creates 3x3 grid)
#' data(olea)
#' olea %>% npoly_calibrate_reconstruction(id = 1, .cols=VD)
#'
#' # Try different degree ranges
#' olea %>% npoly_calibrate_reconstruction(id = 5, degree_range = 3:11, .cols=VD)
#'
#' # Tidyeval column selection
#' olea %>% npoly_calibrate_reconstruction(.cols = VL, id = 1)
#'
#' # Random shape
#' olea %>% npoly_calibrate_reconstruction(.cols=VD)  # samples random shape
#'
#' @seealso [npoly_calibrate_r2()], [npoly()]
#'
#' @export
npoly_calibrate_reconstruction <- function(x,
                                           degree_range = 2:10,
                                           id = NULL,
                                           nb_pts = 120,
                                           .cols = NULL) {

  # Input validation
  if (!is.data.frame(x)) {
    stop("x must be a tibble/data.frame with coo columns")
  }

  # Get coo column to work with (tidyeval support)
  .cols_quo <- rlang::enquo(.cols)
  if (rlang::quo_is_null(.cols_quo)) {
    .cols <- get_coo_cols(x, NULL)
    message(sprintf("Using coo column: '%s'", .cols))
  } else {
    cols_idx <- tidyselect::eval_select(.cols_quo, x)
    .cols <- names(cols_idx)[1]
    message(sprintf("Using coo column: '%s'", .cols))
  }

  # Select shape
  n_shapes <- nrow(x)
  if (is.null(id)) {
    id <- sample(n_shapes, 1)
    message(sprintf("Randomly selected shape %d", id))
  } else {
    if (id > n_shapes || id < 1) {
      stop(sprintf("id must be between 1 and %d", n_shapes))
    }
    message(sprintf("Using shape %d", id))
  }

  # Get original shape (NO scaling)
  coo_orig <- x[[.cols]][[id]]

  # Reconstruct at each degree
  reconstructions <- vector("list", length(degree_range))
  names(reconstructions) <- paste0("degree", degree_range)

  message(sprintf("Reconstructing at degrees %d to %d...",
                  min(degree_range), max(degree_range)))

  for (i in seq_along(degree_range)) {
    deg <- degree_range[i]
    coefs <- .npoly(coo_orig, degree = deg, raw = FALSE)
    reconstructions[[i]] <- .npoly_i(coefs, nb_pts = nb_pts)
  }

  # Create result object
  result <- list(
    data = reconstructions,
    original = coo_orig,
    degree_range = degree_range,
    id = id
  )

  # Add attributes
  attr(result, "method") <- "npoly"
  attr(result, "metric") <- "reconstruction"
  attr(result, "shape_name") <- if (!is.null(x$id)) x$id[id] else paste0("shape_", id)

  # Set class for S3 dispatch
  class(result) <- c("calibrate_reconstruction", "calibrate", "list")

  # Auto-plot
  plot(result)

  invisible(result)
}


# opoly_calibrate_r2 ----

#' Calibrate polynomial degree using R2
#'
#' Calculate R2 (goodness of fit) across a range of polynomial degrees to
#' determine optimal degree for orthogonal polynomial fitting.
#'
#' @param x A tibble with coo columns containing open curves.
#' @param degree_range Integer vector. Range of degrees to test. Default is 2:12.
#' @param id Integer vector or NULL. Which shapes to use for calibration.
#'   If NULL, uses all shapes (or samples if many). If integer vector, uses
#'   those specific row indices.
#' @param thresh Numeric vector. R2 thresholds for recommendations.
#'   Default is c(0.90, 0.95, 0.99).
#' @param .cols Column name(s) to process. If NULL, uses first coo column.
#'   Supports tidyeval (e.g., `.cols = VD`).
#'
#' @return A list with class `c("calibrate_r2", "calibrate")` containing:
#' * `data`: Tibble with columns shape, degree, and r2
#' * `recommended`: Named integer vector of minimum degree per threshold
#' * `summary`: Character string with human-readable recommendation
#'
#' The list has attributes:
#' * `method`: "opoly"
#' * `metric`: "r2"
#' * `param_name`: "degree"
#' * `thresh`: the threshold values used
#'
#' @details
#' This function fits orthogonal polynomials at each degree in `degree_range`
#' for the specified shapes and calculates R2 (proportion of variance explained).
#' Higher degrees always produce higher R2 but may overfit. The goal is to find
#' the minimum degree that achieves acceptable R2.
#'
#' ## Choosing thresholds
#'
#' Common interpretations:
#' * 0.90 (90%): Rough approximation, may miss details
#' * 0.95 (95%): Good balance for most applications
#' * 0.99 (99%): High fidelity, captures fine details
#'
#' ## Sampling strategy
#'
#' When `id = NULL`:
#' * If <= 30 shapes: uses all
#' * If > 30 shapes: randomly samples 30 (set seed for reproducibility)
#'
#' ## Plotting
#'
#' Use `plot(cal)` to visualize results as a boxplot with threshold lines.
#'
#' @examples
#' # Calibrate on subset of olea
#' data(olea)
#' cal <- olea[1:10, ] %>%
#'   opoly_calibrate_r2(.cols=VD)
#'
#' # Examine results
#' cal$recommended
#' cal$summary
#' plot(cal)
#'
#' # Use recommended degree
#' olea %>% opoly(degree = cal$recommended["0.99"])
#'
#' # Tidyeval column selection
#' olea[1:10, ] %>%
#'   opoly_calibrate_r2(.cols = VD)
#'
#' @seealso [opoly_calibrate_reconstruction()], [opoly()]
#'
#' @export
opoly_calibrate_r2 <- function(x,
                               degree_range = 2:12,
                               id = NULL,
                               thresh = c(0.90, 0.95, 0.99),
                               .cols = NULL) {

  # Input validation
  if (!is.data.frame(x)) {
    stop("x must be a tibble/data.frame with coo columns")
  }

  # Get coo column to work with (tidyeval support)
  .cols_quo <- rlang::enquo(.cols)
  if (rlang::quo_is_null(.cols_quo)) {
    .cols <- get_coo_cols(x, NULL)
    message(sprintf("Using coo column: '%s'", .cols))
  } else {
    cols_idx <- tidyselect::eval_select(.cols_quo, x)
    .cols <- names(cols_idx)[1]
    message(sprintf("Using coo column: '%s'", .cols))
  }

  # Determine which shapes to use
  n_shapes <- nrow(x)
  if (is.null(id)) {
    if (n_shapes <= 30) {
      id <- 1:n_shapes
      message(sprintf("Using all %d shapes", n_shapes))
    } else {
      id <- sample(n_shapes, 30)
      message(sprintf("Randomly sampled 30 shapes from %d total", n_shapes))
    }
  } else {
    # Validate id
    if (any(id > n_shapes) || any(id < 1)) {
      stop(sprintf("id values must be between 1 and %d", n_shapes))
    }
    message(sprintf("Using %d specified shapes", length(id)))
  }

  # Prepare result matrix
  n_degrees <- length(degree_range)
  n_id <- length(id)
  res <- matrix(NA, nrow = n_id, ncol = n_degrees)
  rownames(res) <- id
  colnames(res) <- paste0("degree", degree_range)

  # Calculate R2 for each shape and degree
  message(sprintf("Calculating R2 for degrees %d to %d...",
                  min(degree_range), max(degree_range)))

  for (i in seq_along(id)) {
    shape_idx <- id[i]
    coo <- x[[.cols]][[shape_idx]]

    for (j in seq_along(degree_range)) {
      deg <- degree_range[j]
      # Use raw = TRUE to get R2
      fit <- .opoly(coo, degree = deg, raw = TRUE)
      res[i, j] <- fit$r2
    }
  }

  # Convert to tibble for modern handling
  data_tib <- as.data.frame(res)
  data_tib$shape <- rownames(res)
  data_tib <- tidyr::pivot_longer(data_tib,
                                  cols = -shape,
                                  names_to = "degree",
                                  values_to = "r2")
  data_tib$degree <- as.integer(sub("degree", "", data_tib$degree))
  data_tib <- dplyr::arrange(data_tib, shape, degree)

  # Calculate median R2 per degree
  med_r2 <- apply(res, 2, median)

  # Find minimum degree for each threshold
  recommended <- integer(length(thresh))
  names(recommended) <- as.character(thresh)

  for (i in seq_along(thresh)) {
    meets_thresh <- which(med_r2 >= thresh[i])
    if (length(meets_thresh) == 0) {
      recommended[i] <- NA
      warning(sprintf("No degree achieves R2 >= %.3f", thresh[i]))
    } else {
      recommended[i] <- degree_range[min(meets_thresh)]
    }
  }

  # Generate summary text
  summary_lines <- character(length(thresh))
  for (i in seq_along(thresh)) {
    if (!is.na(recommended[i])) {
      summary_lines[i] <- sprintf("Use degree >= %d for %.0f%% R2",
                                  recommended[i], thresh[i] * 100)
    } else {
      summary_lines[i] <- sprintf("No degree achieves %.0f%% R2 in tested range",
                                  thresh[i] * 100)
    }
  }
  summary_text <- paste(summary_lines, collapse = "\n")

  # Create result object
  result <- list(
    data = data_tib,
    recommended = recommended,
    summary = summary_text
  )

  # Add attributes for plotting
  attr(result, "method") <- "opoly"
  attr(result, "metric") <- "r2"
  attr(result, "param_name") <- "degree"
  attr(result, "value_name") <- "r2"
  attr(result, "thresh") <- thresh

  # Set class for S3 dispatch
  class(result) <- c("calibrate_r2", "calibrate", "list")

  # Print summary
  cat("\n")
  cat(summary_text, "\n")
  cat("\nUse plot(result) to visualize\n")

  invisible(result)
}


# opoly_calibrate_reconstruction ----

#' Calibrate polynomial degree using visual reconstruction
#'
#' Visually compare reconstructed shapes at different polynomial degrees to
#' assess fit quality and determine optimal degree.
#'
#' @param x A tibble with coo columns containing open curves.
#' @param degree_range Integer vector. Range of degrees to display. Default is 2:10 (9 panels).
#' @param id Integer or NULL. Which shape to display. If NULL, randomly samples one.
#' @param nb_pts Integer. Number of points for reconstruction. Default is 120.
#' @param .cols Column name(s) to process. If NULL, uses first coo column.
#'   Supports tidyeval (e.g., `.cols = VD`).
#'
#' @return A list with class `c("calibrate_reconstruction", "calibrate")` containing:
#' * `data`: List of reconstructed coordinates at each degree
#' * `original`: Original shape coordinates (not scaled)
#' * `degree_range`: The degrees displayed
#' * `id`: Index of shape used
#'
#' The list has attributes:
#' * `method`: "opoly"
#' * `metric`: "reconstruction"
#'
#' @details
#' This function creates a multi-panel plot (3x3 grid for default 9 degrees) showing:
#' * Original shape in black
#' * Reconstructed shape in red
#' * One panel per degree in `degree_range`
#' * Visual assessment of how reconstruction improves with degree
#'
#' This is the most intuitive calibration method - you can directly see when
#' adding more degrees stops meaningfully improving the fit.
#'
#' @examples
#' # Calibrate on single shape from olea (creates 3x3 grid)
#' data(olea)
#' olea[1:10, ] %>% opoly_calibrate_reconstruction(id = 1, .cols=VD)
#'
#' # Try different degree ranges
#' olea[1:10,] %>% opoly_calibrate_reconstruction(id = 5, degree_range = 3:11, .cols=VD)
#'
#' # Tidyeval column selection
#' olea[1:10, ] %>% opoly_calibrate_reconstruction(.cols = VL, id = 1)
#'
#' # Random shape
#' olea %>% opoly_calibrate_reconstruction(.cols=VD)  # samples random shape
#'
#' @seealso [opoly_calibrate_r2()], [opoly()]
#'
#' @export
opoly_calibrate_reconstruction <- function(x,
                                           degree_range = 2:10,
                                           id = NULL,
                                           nb_pts = 120,
                                           .cols = NULL) {

  # Input validation
  if (!is.data.frame(x)) {
    stop("x must be a tibble/data.frame with coo columns")
  }

  # Get coo column to work with (tidyeval support)
  .cols_quo <- rlang::enquo(.cols)
  if (rlang::quo_is_null(.cols_quo)) {
    .cols <- get_coo_cols(x, NULL)
    message(sprintf("Using coo column: '%s'", .cols))
  } else {
    cols_idx <- tidyselect::eval_select(.cols_quo, x)
    .cols <- names(cols_idx)[1]
    message(sprintf("Using coo column: '%s'", .cols))
  }

  # Select shape
  n_shapes <- nrow(x)
  if (is.null(id)) {
    id <- sample(n_shapes, 1)
    message(sprintf("Randomly selected shape %d", id))
  } else {
    if (id > n_shapes || id < 1) {
      stop(sprintf("id must be between 1 and %d", n_shapes))
    }
    message(sprintf("Using shape %d", id))
  }

  # Get original shape (NO scaling)
  coo_orig <- x[[.cols]][[id]]

  # Reconstruct at each degree
  reconstructions <- vector("list", length(degree_range))
  names(reconstructions) <- paste0("degree", degree_range)

  message(sprintf("Reconstructing at degrees %d to %d...",
                  min(degree_range), max(degree_range)))

  for (i in seq_along(degree_range)) {
    deg <- degree_range[i]
    coefs <- .opoly(coo_orig, degree = deg, raw = FALSE)
    reconstructions[[i]] <- .opoly_i(coefs, nb_pts = nb_pts)
  }

  # Create result object
  result <- list(
    data = reconstructions,
    original = coo_orig,
    degree_range = degree_range,
    id = id
  )

  # Add attributes
  attr(result, "method") <- "opoly"
  attr(result, "metric") <- "reconstruction"
  attr(result, "shape_name") <- if (!is.null(x$id)) x$id[id] else paste0("shape_", id)

  # Set class for S3 dispatch
  class(result) <- c("calibrate_reconstruction", "calibrate", "list")

  # Auto-plot
  plot(result)

  invisible(result)
}


# eft_calibrate_harmonicpower ----

#' Calibrate number of harmonics using harmonic power
#'
#' Calculate cumulative harmonic power across a range of harmonics to
#' determine optimal number for elliptic Fourier transform.
#'
#' @param x A tibble with coo columns containing closed outlines.
#' @param nb_h Integer. Maximum number of harmonics to compute. Default is 20.
#' @param id Integer vector or NULL. Which shapes to use for calibration.
#'   If NULL, uses all shapes (or samples if many). If integer vector, uses
#'   those specific row indices.
#' @param thresh Numeric vector. Cumulative power thresholds (percentage) for
#'   recommendations. Default is c(90, 95, 99).
#' @param .cols Column name(s) to process. If NULL, uses first coo column.
#'   Supports tidyeval (e.g., `.cols = VD`).
#'
#' @return A list with class `c("calibrate_harmonicpower", "calibrate")` containing:
#' * `data`: Tibble with columns shape, harmonic, and power
#' * `recommended`: Named integer vector of minimum harmonics per threshold
#' * `summary`: Character string with human-readable recommendation
#'
#' The list has attributes:
#' * `method`: "eft"
#' * `metric`: "harmonicpower"
#' * `param_name`: "harmonic"
#' * `thresh`: the threshold values used
#'
#' @details
#' Harmonic power measures how much shape variation each harmonic captures.
#' Cumulative power reaches 100% asymptotically. The goal is to find the
#' minimum number of harmonics that captures sufficient variation (e.g., 99%).
#'
#' ## Choosing thresholds
#'
#' Common interpretations:
#' * 90%: Captures major shape features
#' * 95%: Good balance for most applications
#' * 99%: High fidelity, captures fine details
#'
#' ## Sampling strategy
#'
#' When `id = NULL`:
#' * If <= 30 shapes: uses all
#' * If > 30 shapes: randomly samples 30
#'
#' ## Plotting
#'
#' Use `plot(cal)` to visualize results as cumulative power curves.
#'
#' @examples
#' # Calibrate on subset of olea
#' data(bot)
#' cal <- bot[1:10, ] %>%
#'   eft_calibrate_harmonicpower()
#'
#' cal$recommended
#' cal$summary
#' plot(cal)
#'
#' # Use recommended harmonics
#' bot[1:10,] %>% eft(nb_h = cal$recommended["99"])
#'
#' @seealso [eft_calibrate_reconstruction()], [eft()]
#'
#' @export
eft_calibrate_harmonicpower <- function(x,
                                        nb_h = 20,
                                        id = NULL,
                                        thresh = c(90, 95, 99),
                                        .cols = NULL) {

  # Input validation
  if (!is.data.frame(x)) {
    stop("x must be a tibble/data.frame with coo columns")
  }

  # Get coo column to work with (tidyeval support)
  .cols_quo <- rlang::enquo(.cols)
  if (rlang::quo_is_null(.cols_quo)) {
    .cols <- get_coo_cols(x, NULL)
    message(sprintf("Using coo column: '%s'", .cols))
  } else {
    cols_idx <- tidyselect::eval_select(.cols_quo, x)
    .cols <- names(cols_idx)[1]
    message(sprintf("Using coo column: '%s'", .cols))
  }

  # Determine which shapes to use
  n_shapes <- nrow(x)
  if (is.null(id)) {
    if (n_shapes <= 30) {
      id <- 1:n_shapes
      message(sprintf("Using all %d shapes", n_shapes))
    } else {
      id <- sample(n_shapes, 30)
      message(sprintf("Randomly sampled 30 shapes from %d total", n_shapes))
    }
  } else {
    if (any(id > n_shapes) || any(id < 1)) {
      stop(sprintf("id values must be between 1 and %d", n_shapes))
    }
    message(sprintf("Using %d specified shapes", length(id)))
  }

  # Prepare result matrix
  n_id <- length(id)
  res <- matrix(NA, nrow = n_id, ncol = nb_h)
  rownames(res) <- id
  colnames(res) <- paste0("h", 1:nb_h)

  # Calculate harmonic power for each shape
  message(sprintf("Calculating harmonic power for %d harmonics...", nb_h))

  for (i in seq_along(id)) {
    shape_idx <- id[i]
    coo <- x[[.cols]][[shape_idx]]

    # Compute EFT with raw = TRUE to get modulus
    eft_result <- .eft(coo, nb_h = nb_h, raw = TRUE)

    # Harmonic power is the square of modulus
    harm_power <- (eft_result$an^2+eft_result$bn^2+eft_result$cn^2+eft_result$dn^2)/2
    res[i, ] <- harm_power
  }

  # Calculate cumulative power (as percentage)
  res_cumsum <- t(apply(res, 1, function(x) cumsum(x) / sum(x) * 100))

  # Convert to tibble
  data_tib <- as.data.frame(res_cumsum)
  data_tib$shape <- rownames(res_cumsum)
  data_tib <- tidyr::pivot_longer(data_tib,
                                  cols = -shape,
                                  names_to = "harmonic",
                                  values_to = "power")
  data_tib$harmonic <- as.integer(sub("h", "", data_tib$harmonic))
  data_tib <- dplyr::arrange(data_tib, shape, harmonic)

  # Calculate median cumulative power per harmonic
  med_power <- apply(res_cumsum, 2, median)

  # Find minimum harmonics for each threshold
  recommended <- integer(length(thresh))
  names(recommended) <- as.character(thresh)

  for (i in seq_along(thresh)) {
    meets_thresh <- which(med_power >= thresh[i])
    if (length(meets_thresh) == 0) {
      recommended[i] <- NA
      warning(sprintf("No harmonic achieves %.0f%% power", thresh[i]))
    } else {
      recommended[i] <- min(meets_thresh)
    }
  }

  # Generate summary text
  summary_lines <- character(length(thresh))
  for (i in seq_along(thresh)) {
    if (!is.na(recommended[i])) {
      summary_lines[i] <- sprintf("Use nb_h >= %d for %.0f%% cumulative power",
                                  recommended[i], thresh[i])
    } else {
      summary_lines[i] <- sprintf("No harmonic achieves %.0f%% power in tested range",
                                  thresh[i])
    }
  }
  summary_text <- paste(summary_lines, collapse = "\n")

  # Create result object
  result <- list(
    data = data_tib,
    recommended = recommended,
    summary = summary_text
  )

  # Add attributes for plotting
  attr(result, "method") <- "eft"
  attr(result, "metric") <- "harmonicpower"
  attr(result, "param_name") <- "harmonic"
  attr(result, "value_name") <- "power"
  attr(result, "thresh") <- thresh

  # Set class for S3 dispatch
  class(result) <- c("calibrate_harmonicpower", "calibrate", "list")

  # Print summary
  cat("\n")
  cat(summary_text, "\n")
  cat("\nUse plot(result) to visualize\n")

  invisible(result)
}


# eft_calibrate_reconstruction ----

#' Calibrate number of harmonics using visual reconstruction
#'
#' Visually compare reconstructed shapes at different harmonic numbers to
#' assess fit quality and determine optimal nb_h.
#'
#' @param x A tibble with coo columns containing closed outlines.
#' @param nb_h_range Integer vector. Range of harmonics to display. Default is 2:10 (9 panels).
#' @param id Integer or NULL. Which shape to display. If NULL, randomly samples one.
#' @param nb_pts Integer. Number of points for reconstruction. Default is 60.
#' @param .cols Column name(s) to process. If NULL, uses first coo column.
#'   Supports tidyeval (e.g., `.cols = VD`).
#'
#' @return A list with class `c("calibrate_reconstruction", "calibrate")` containing:
#' * `data`: List of reconstructed coordinates at each harmonic level
#' * `original`: Original shape coordinates (not scaled)
#' * `nb_h_range`: The harmonics displayed
#' * `id`: Index of shape used
#'
#' The list has attributes:
#' * `method`: "eft"
#' * `metric`: "reconstruction"
#'
#' @details
#' This function creates a multi-panel plot (3x3 grid for default 9 harmonics) showing:
#' * Original shape in black
#' * Reconstructed shape in red
#' * One panel per harmonic level in `nb_h_range`
#' * Visual assessment of how reconstruction improves with harmonics
#'
#' @examples
#' # Calibrate on single shape from olea
#' data(olea)
#' olea %>% eft_calibrate_reconstruction(id = 1, .cols=VD)
#'
#' # Try different harmonic ranges
#' olea %>% eft_calibrate_reconstruction(id = 5, nb_h_range = 3:11, .cols=VD)
#'
#' # Tidyeval column selection
#' olea %>% eft_calibrate_reconstruction(.cols = VL, id = 1)
#'
#' @seealso [eft_calibrate_harmonicpower()], [eft()]
#'
#' @export
eft_calibrate_reconstruction <- function(x,
                                         nb_h_range = 2:10,
                                         id = NULL,
                                         nb_pts = 60,
                                         .cols = NULL) {

  # Input validation
  if (!is.data.frame(x)) {
    stop("x must be a tibble/data.frame with coo columns")
  }

  # Get coo column to work with (tidyeval support)
  .cols_quo <- rlang::enquo(.cols)
  if (rlang::quo_is_null(.cols_quo)) {
    .cols <- get_coo_cols(x, NULL)
    message(sprintf("Using coo column: '%s'", .cols))
  } else {
    cols_idx <- tidyselect::eval_select(.cols_quo, x)
    .cols <- names(cols_idx)[1]
    message(sprintf("Using coo column: '%s'", .cols))
  }

  # Select shape
  n_shapes <- nrow(x)
  if (is.null(id)) {
    id <- sample(n_shapes, 1)
    message(sprintf("Randomly selected shape %d", id))
  } else {
    if (id > n_shapes || id < 1) {
      stop(sprintf("id must be between 1 and %d", n_shapes))
    }
    message(sprintf("Using shape %d", id))
  }

  # Get original shape (NO scaling)
  coo_orig <- x[[.cols]][[id]]

  # Reconstruct at each harmonic level
  reconstructions <- vector("list", length(nb_h_range))
  names(reconstructions) <- paste0("h", nb_h_range)

  message(sprintf("Reconstructing at harmonics %d to %d...",
                  min(nb_h_range), max(nb_h_range)))

  for (i in seq_along(nb_h_range)) {
    nh <- nb_h_range[i]
    coefs <- .eft(coo_orig, nb_h = nh, raw = FALSE)
    reconstructions[[i]] <- .eft_i(coefs, nb_h = nh, nb_pts = nb_pts)
  }

  # Create result object
  result <- list(
    data = reconstructions,
    original = coo_center(coo_orig),
    nb_h_range = nb_h_range,
    id = id
  )

  # Add attributes
  attr(result, "method") <- "eft"
  attr(result, "metric") <- "reconstruction"
  attr(result, "param_name") <- "harmonic"
  attr(result, "shape_name") <- if (!is.null(x$id)) x$id[id] else paste0("shape_", id)

  # Set class for S3 dispatch
  class(result) <- c("calibrate_reconstruction", "calibrate", "list")

  # Auto-plot
  plot(result)

  invisible(result)
}


# dct_calibrate_harmonicpower ----

#' Calibrate number of harmonics using harmonic power
#'
#' Calculate cumulative harmonic power across a range of harmonics to
#' determine optimal number for discrete cosine transform.
#'
#' @param x A tibble with coo columns containing open curves.
#' @param nb_h Integer. Maximum number of harmonics to compute. Default is 20.
#' @param id Integer vector or NULL. Which shapes to use for calibration.
#'   If NULL, uses all shapes (or samples if many). If integer vector, uses
#'   those specific row indices.
#' @param thresh Numeric vector. Cumulative power thresholds (percentage) for
#'   recommendations. Default is c(90, 95, 99).
#' @param .cols Column name(s) to process. If NULL, uses first coo column.
#'   Supports tidyeval (e.g., `.cols = VD`).
#'
#' @return A list with class `c("calibrate_harmonicpower", "calibrate")` containing:
#' * `data`: Tibble with columns shape, harmonic, and power
#' * `recommended`: Named integer vector of minimum harmonics per threshold
#' * `summary`: Character string with human-readable recommendation
#'
#' The list has attributes:
#' * `method`: "dct"
#' * `metric`: "harmonicpower"
#' * `param_name`: "harmonic"
#' * `thresh`: the threshold values used
#'
#' @details
#' Harmonic power measures how much shape variation each harmonic captures.
#' Cumulative power reaches 100% asymptotically. The goal is to find the
#' minimum number of harmonics that captures sufficient variation (e.g., 99%).
#'
#' ## Choosing thresholds
#'
#' Common interpretations:
#' * 90%: Captures major shape features
#' * 95%: Good balance for most applications
#' * 99%: High fidelity, captures fine details
#'
#' ## Sampling strategy
#'
#' When `id = NULL`:
#' * If <= 30 shapes: uses all
#' * If > 30 shapes: randomly samples 30
#'
#' ## Plotting
#'
#' Use `plot(cal)` to visualize results as cumulative power curves.
#'
#' @examples
#' # Calibrate on subset of olea
#' data(olea)
#' cal <- olea[1:10, ] %>%
#'   dct_calibrate_harmonicpower(.cols=VD)
#'
#' cal$recommended
#' cal$summary
#' plot(cal)
#'
#' # Use recommended harmonics
#' olea %>% dct(nb_h = cal$recommended["99"])
#'
#' @seealso [dct_calibrate_reconstruction()], [dct()]
#'
#' @export
dct_calibrate_harmonicpower <- function(x,
                                        nb_h = 20,
                                        id = NULL,
                                        thresh = c(90, 95, 99),
                                        .cols = NULL) {

  # Input validation
  if (!is.data.frame(x)) {
    stop("x must be a tibble/data.frame with coo columns")
  }

  # Get coo column to work with (tidyeval support)
  .cols_quo <- rlang::enquo(.cols)
  if (rlang::quo_is_null(.cols_quo)) {
    .cols <- get_coo_cols(x, NULL)
    message(sprintf("Using coo column: '%s'", .cols))
  } else {
    cols_idx <- tidyselect::eval_select(.cols_quo, x)
    .cols <- names(cols_idx)[1]
    message(sprintf("Using coo column: '%s'", .cols))
  }

  # Determine which shapes to use
  n_shapes <- nrow(x)
  if (is.null(id)) {
    if (n_shapes <= 30) {
      id <- 1:n_shapes
      message(sprintf("Using all %d shapes", n_shapes))
    } else {
      id <- sample(n_shapes, 30)
      message(sprintf("Randomly sampled 30 shapes from %d total", n_shapes))
    }
  } else {
    if (any(id > n_shapes) || any(id < 1)) {
      stop(sprintf("id values must be between 1 and %d", n_shapes))
    }
    message(sprintf("Using %d specified shapes", length(id)))
  }

  # Prepare result matrix
  n_id <- length(id)
  res <- matrix(NA, nrow = n_id, ncol = nb_h)
  rownames(res) <- id
  colnames(res) <- paste0("h", 1:nb_h)

  # Calculate harmonic power for each shape
  message(sprintf("Calculating harmonic power for %d harmonics...", nb_h))

  for (i in seq_along(id)) {
    shape_idx <- id[i]
    coo <- x[[.cols]][[shape_idx]]

    # Compute DCT with raw = TRUE to get modulus
    dct_result <- .dct(coo, nb_h = nb_h, raw = TRUE)

    # Harmonic power is the square of modulus
    harm_power <- dct_result$mod^2
    res[i, ] <- harm_power
  }

  # Calculate cumulative power (as percentage)
  res_cumsum <- t(apply(res, 1, function(x) cumsum(x) / sum(x) * 100))

  # Convert to tibble
  data_tib <- as.data.frame(res_cumsum)
  data_tib$shape <- rownames(res_cumsum)
  data_tib <- tidyr::pivot_longer(data_tib,
                                  cols = -shape,
                                  names_to = "harmonic",
                                  values_to = "power")
  data_tib$harmonic <- as.integer(sub("h", "", data_tib$harmonic))
  data_tib <- dplyr::arrange(data_tib, shape, harmonic)

  # Calculate median cumulative power per harmonic
  med_power <- apply(res_cumsum, 2, median)

  # Find minimum harmonics for each threshold
  recommended <- integer(length(thresh))
  names(recommended) <- as.character(thresh)

  for (i in seq_along(thresh)) {
    meets_thresh <- which(med_power >= thresh[i])
    if (length(meets_thresh) == 0) {
      recommended[i] <- NA
      warning(sprintf("No harmonic achieves %.0f%% power", thresh[i]))
    } else {
      recommended[i] <- min(meets_thresh)
    }
  }

  # Generate summary text
  summary_lines <- character(length(thresh))
  for (i in seq_along(thresh)) {
    if (!is.na(recommended[i])) {
      summary_lines[i] <- sprintf("Use nb_h >= %d for %.0f%% cumulative power",
                                  recommended[i], thresh[i])
    } else {
      summary_lines[i] <- sprintf("No harmonic achieves %.0f%% power in tested range",
                                  thresh[i])
    }
  }
  summary_text <- paste(summary_lines, collapse = "\n")

  # Create result object
  result <- list(
    data = data_tib,
    recommended = recommended,
    summary = summary_text
  )

  # Add attributes for plotting
  attr(result, "method") <- "dct"
  attr(result, "metric") <- "harmonicpower"
  attr(result, "param_name") <- "harmonic"
  attr(result, "value_name") <- "power"
  attr(result, "thresh") <- thresh

  # Set class for S3 dispatch
  class(result) <- c("calibrate_harmonicpower", "calibrate", "list")

  # Print summary
  cat("\n")
  cat(summary_text, "\n")
  cat("\nUse plot(result) to visualize\n")

  invisible(result)
}


# dct_calibrate_reconstruction ----

#' Calibrate number of harmonics using visual reconstruction
#'
#' Visually compare reconstructed shapes at different harmonic numbers to
#' assess fit quality and determine optimal nb_h.
#'
#' @param x A tibble with coo columns containing open curves.
#' @param nb_h_range Integer vector. Range of harmonics to display. Default is 2:10 (9 panels).
#' @param id Integer or NULL. Which shape to display. If NULL, randomly samples one.
#' @param nb_pts Integer. Number of points for reconstruction. Default is 60.
#' @param .cols Column name(s) to process. If NULL, uses first coo column.
#'   Supports tidyeval (e.g., `.cols = VD`).
#'
#' @return A list with class `c("calibrate_reconstruction", "calibrate")` containing:
#' * `data`: List of reconstructed coordinates at each harmonic level
#' * `original`: Original shape coordinates (not scaled)
#' * `nb_h_range`: The harmonics displayed
#' * `id`: Index of shape used
#'
#' The list has attributes:
#' * `method`: "dct"
#' * `metric`: "reconstruction"
#'
#' @details
#' This function creates a multi-panel plot (3x3 grid for default 9 harmonics) showing:
#' * Original shape in black
#' * Reconstructed shape in red
#' * One panel per harmonic level in `nb_h_range`
#' * Visual assessment of how reconstruction improves with harmonics
#'
#' @examples
#' # Calibrate on single shape from olea
#' data(olea)
#' olea %>% dct_calibrate_reconstruction(id = 1, .cols=VL)
#'
#' # Try different harmonic ranges
#' olea %>% dct_calibrate_reconstruction(id = 5, nb_h_range = 3:11, .cols=VL)
#'
#' # Tidyeval column selection
#' olea %>% dct_calibrate_reconstruction(.cols = VL, id = 1)
#'
#' @seealso [dct_calibrate_harmonicpower()], [dct()]
#'
#' @export
dct_calibrate_reconstruction <- function(x,
                                         nb_h_range = 2:10,
                                         id = NULL,
                                         nb_pts = 60,
                                         .cols = NULL) {

  # Input validation
  if (!is.data.frame(x)) {
    stop("x must be a tibble/data.frame with coo columns")
  }

  # Get coo column to work with (tidyeval support)
  .cols_quo <- rlang::enquo(.cols)
  if (rlang::quo_is_null(.cols_quo)) {
    .cols <- get_coo_cols(x, NULL)
    message(sprintf("Using coo column: '%s'", .cols))
  } else {
    cols_idx <- tidyselect::eval_select(.cols_quo, x)
    .cols <- names(cols_idx)[1]
    message(sprintf("Using coo column: '%s'", .cols))
  }

  # Select shape
  n_shapes <- nrow(x)
  if (is.null(id)) {
    id <- sample(n_shapes, 1)
    message(sprintf("Randomly selected shape %d", id))
  } else {
    if (id > n_shapes || id < 1) {
      stop(sprintf("id must be between 1 and %d", n_shapes))
    }
    message(sprintf("Using shape %d", id))
  }

  # Get original shape (NO scaling)
  coo_orig <- x[[.cols]][[id]]

  # Reconstruct at each harmonic level
  reconstructions <- vector("list", length(nb_h_range))
  names(reconstructions) <- paste0("h", nb_h_range)

  message(sprintf("Reconstructing at harmonics %d to %d...",
                  min(nb_h_range), max(nb_h_range)))

  for (i in seq_along(nb_h_range)) {
    nh <- nb_h_range[i]
    coefs <- .dct(coo_orig, nb_h = nh, raw = FALSE)
    reconstructions[[i]] <- .dct_i(coefs, nb_h = nh, nb_pts = nb_pts)
  }

  # Create result object
  result <- list(
    data = reconstructions,
    original = coo_orig,
    nb_h_range = nb_h_range,
    id = id
  )

  # Add attributes
  attr(result, "method") <- "dct"
  attr(result, "metric") <- "reconstruction"
  attr(result, "param_name") <- "harmonic"
  attr(result, "shape_name") <- if (!is.null(x$id)) x$id[id] else paste0("shape_", id)

  # Set class for S3 dispatch
  class(result) <- c("calibrate_reconstruction", "calibrate", "list")

  # Auto-plot
  plot(result)

  invisible(result)
}


# Plot methods ----
# All plot methods are defined here with @rdname plot_calibrate
# This creates a single documentation page for all plot methods

#' Plot calibration results
#'
#' S3 plot methods for calibration objects. The appropriate method is
#' automatically dispatched based on the calibration type.
#'
#' @param x A calibration object from one of the calibration functions:
#'   * `calibrate_r2` from [opoly_calibrate_r2()], [npoly_calibrate_r2()]
#'   * `calibrate_harmonicpower` from [dct_calibrate_harmonicpower()], [eft_calibrate_harmonicpower()]
#'   * `calibrate_reconstruction` from [opoly_calibrate_reconstruction()],
#'     [npoly_calibrate_reconstruction()], [dct_calibrate_reconstruction()],
#'     [eft_calibrate_reconstruction()]
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns the input object.
#'
#' @details
#' ## plot.calibrate_r2
#'
#' Creates a boxplot showing R2 values across degrees with:
#' * X-axis: degrees
#' * Y-axis: R2 values
#' * Horizontal dashed lines for thresholds (90%, 95%, 99%)
#' * Color-coded: green=90%, orange=95%, red=99%
#'
#' ## plot.calibrate_harmonicpower
#'
#' Creates a boxplot showing cumulative harmonic power with:
#' * X-axis: harmonic number
#' * Y-axis: cumulative power (%)
#' * Horizontal dashed lines for thresholds
#' * Shows how shape variation is captured by each harmonic
#'
#' ## plot.calibrate_reconstruction
#'
#' Creates a multi-panel plot (typically 3x3 grid) with:
#' * One panel per parameter value (degree or harmonic)
#' * Original shape in black (lwd=1.5)
#' * Reconstructed shape in red (lwd=1)
#' * Equal aspect ratio for accurate shape display
#' * Visual assessment of reconstruction quality
#'
#' @examples
#' # R2 calibration
#' data(olea)
#' cal_r2 <- olea[1:5,] %>% opoly_calibrate_r2(.cols=VD)
#' plot(cal_r2)
#'
#' # Harmonic power calibration
#' cal_power <- olea[1:5,] %>% dct_calibrate_harmonicpower(.cols=VL)
#' plot(cal_power)
#'
#' # Reconstruction calibration
#' cal_recon <- olea[1:5,] %>% opoly_calibrate_reconstruction(id = 1, .cols=VL)
#' plot(cal_recon)  # Already plotted automatically
#'
#' @name plot_calibrate
NULL

#' @rdname plot_calibrate
#' @export
plot.calibrate_r2 <- function(x, ...) {
  # Extract attributes
  method <- attr(x, "method")
  param_name <- attr(x, "param_name")
  value_name <- attr(x, "value_name")
  thresh <- attr(x, "thresh")

  # Get data
  data <- x$data

  # Split data by parameter for boxplot
  param_values <- sort(unique(data[[param_name]]))
  plot_data <- split(data[[value_name]], data[[param_name]])

  # Create boxplot
  boxplot(plot_data,
          xlab = tools::toTitleCase(param_name),
          ylab = "R2",
          main = sprintf("R2 calibration for %s", method),
          col = "lightblue",
          border = "darkblue",
          names = param_values)

  # Add horizontal lines for thresholds
  thresh_cols <- c("darkgreen", "orange", "red")[1:length(thresh)]
  for (i in seq_along(thresh)) {
    abline(h = thresh[i], col = thresh_cols[i], lty = 2, lwd = 1.5)
    text(x = par("usr")[1] + 0.5, y = thresh[i],
         labels = sprintf("%.0f%%", thresh[i] * 100),
         pos = 3, col = thresh_cols[i], cex = 0.8)
  }

  # Add grid
  grid(nx = NA, ny = NULL, col = "gray90", lty = 1)

  invisible(x)
}

#' @rdname plot_calibrate
#' @export
plot.calibrate_harmonicpower <- function(x, ...) {
  # Extract attributes
  method <- attr(x, "method")
  param_name <- attr(x, "param_name")
  value_name <- attr(x, "value_name")
  thresh <- attr(x, "thresh")

  # Get data
  data <- x$data

  # Split data by parameter for boxplot
  param_values <- sort(unique(data[[param_name]]))
  plot_data <- split(data[[value_name]], data[[param_name]])

  # Create boxplot
  boxplot(plot_data,
          xlab = tools::toTitleCase(param_name),
          ylab = "Cumulative power (%)",
          main = sprintf("Harmonic power calibration for %s", method),
          col = "lightblue",
          border = "darkblue",
          names = param_values)

  # Add horizontal lines for thresholds
  thresh_cols <- c("darkgreen", "orange", "red")[1:length(thresh)]
  for (i in seq_along(thresh)) {
    abline(h = thresh[i], col = thresh_cols[i], lty = 2, lwd = 1.5)
    text(x = par("usr")[1] + 0.5, y = thresh[i],
         labels = sprintf("%.0f%%", thresh[i]),
         pos = 3, col = thresh_cols[i], cex = 0.8)
  }

  # Add grid
  grid(nx = NA, ny = NULL, col = "gray90", lty = 1)

  invisible(x)
}

#' @rdname plot_calibrate
#' @export
plot.calibrate_reconstruction <- function(x, ...) {
  # Extract components
  method <- attr(x, "method")
  param_name <- attr(x, "param_name")
  shape_name <- attr(x, "shape_name")
  reconstructions <- x$data
  original <- x$original

  # Get parameter range (works for both degree_range and nb_h_range)
  param_range <- if (!is.null(x$degree_range)) {
    x$degree_range
  } else {
    x$nb_h_range
  }

  # Calculate layout
  n_panels <- length(param_range)
  n_cols <- ceiling(sqrt(n_panels))
  n_rows <- ceiling(n_panels / n_cols)

  # Set up multi-panel plot
  old_par <- par(mfrow = c(n_rows, n_cols),
                 mar = c(2, 2, 2, 1),
                 oma = c(0, 0, 3, 0))
  on.exit(par(old_par))

  # Get coordinate ranges for consistent axes
  all_coords <- rbind(original, do.call(rbind, reconstructions))
  xlim <- range(all_coords[, 1])
  ylim <- range(all_coords[, 2])

  # Plot each reconstruction
  for (i in seq_along(param_range)) {
    param_val <- param_range[i]
    recon <- reconstructions[[i]]

    # Create title based on method
    if (method %in% c("opoly", "npoly")) {
      title_text <- sprintf("Degree %d", param_val)
    } else {
      title_text <- sprintf("%d harmonics", param_val)
    }

    # Empty plot with consistent limits
    plot(original,
         type = "n",
         xlim = xlim,
         ylim = ylim,
         asp = 1,
         xlab = "",
         ylab = "",
         main = title_text)

    # Plot original in black
    lines(original, col = "black", lwd = 1.5)

    # Plot reconstruction in red
    lines(recon, col = "red", lwd = 1)
  }

  # Add overall title
  title_text <- sprintf("Reconstruction calibration for %s - %s",
                        method, shape_name)
  mtext(title_text, outer = TRUE, cex = 1.2, font = 2)

  invisible(x)
}

