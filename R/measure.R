# measure ----

#' Measure shape properties
#'
#' Add measurement columns to a tibble by computing scalar shape properties.
#'
#' @param x A tibble with coo columns.
#' @param measures Character vector of measurement names to compute.
#'   See Details for available measurements.
#' @param ... Additional arguments passed to measurement functions.
#' @param .cols Column name(s) to process. If `NULL`, automatically detects
#'   columns containing coo objects.
#' @param .prefix Character. Custom prefix for new column names. If `NULL`,
#'   uses "colname_measurename" (e.g., "coo_area").
#'
#' @return The input tibble with new measurement column(s) added.
#'
#' @details
#' Available measurements (all require scalar-returning `get_*` functions):
#'
#' **Size and perimeter:**
#' - `"area"` - polygon area (shoelace formula)
#' - `"perim"` - perimeter length
#'
#' **Centroid size:**
#' - `"centroid_size"` - centroid size (standard)
#' - `"centroid_size_norm"` - normalized centroid size
#'
#' **Circularity:**
#' - `"circularity"` - isoperimetric quotient (4πA/P²)
#' - `"circularity_norm"` - normalized circularity
#' - `"circularity_haralick"` - mean(radii)/sd(radii) from centroid
#'
#' **Length and aspect ratio:**
#' - `"length"` - range along major inertia axis
#' - `"width"` - range along minor inertia axis
#' - `"elongation"` - aspect ratio (length/width)
#' - `"rectangularity"` - area/(length×width)
#'
#' **Convex hull measures:**
#' - `"convexity"` - perim_hull/perim
#' - `"solidity"` - area/area_hull
#'
#' **Complex shape descriptors:**
#' - `"rectilinearity"` - rectangular fit after optimal rotation
#' - `"calliper"` - maximum distance between points
#'
#' To see all available measurements, use `available_measures()`.
#'
#' @examples
#' # Single measurement
#' bot %>% measure("area")
#'
#' # Multiple measurements
#' bot %>% measure(c("area", "perim", "centroid_size"))
#'
#' # Custom prefix
#' bot %>% measure(c("area", "perim"), .prefix = "shape")
#'
#' # On specific column
#' bot %>% measure("area", .cols = coo)
#'
#' @seealso [available_measures()] for list of measurements; `get_*` functions for extraction
#'
#' @keywords internal
#' @export
measure <- function(x, measures, ..., .cols = NULL, .prefix = NULL) {
  # Only works on tibbles
  if (!is.data.frame(x)) {
    stop("measure() only works on tibbles/data.frames. Use get_*() functions for matrices/lists.")
  }

  # Get coo column(s) with tidyeval
  .cols_quo <- rlang::enquo(.cols)
  if (rlang::quo_is_null(.cols_quo)) {
    cols_to_process <- get_coo_cols(x, NULL)
  } else {
    cols_idx <- tidyselect::eval_select(.cols_quo, x)
    cols_to_process <- names(cols_idx)
  }

  # Process each measurement
  for (measure_name in measures) {
    # Get the getter function
    getter_fn <- .get_measure_function(measure_name)

    # For each coo column
    for (col in cols_to_process) {
      # Determine column name
      if (is.null(.prefix)) {
        new_col_name <- paste0(col, "_", measure_name)
      } else {
        new_col_name <- paste0(.prefix, "_", measure_name)
      }

      # Call getter and add column
      x[[new_col_name]] <- vapply(x[[col]], getter_fn, numeric(1), ...)
    }
  }

  x
}


# available_measures ----

#' List available measurements
#'
#' Show all measurements that can be computed with `measure()`.
#'
#' @return Character vector of available measurement names.
#'
#' @details
#' Returns names of all scalar-returning `get_*` functions that can be
#' used with `measure()`.
#'
#' @examples
#' available_measures()
#'
#' @keywords internal
#' @export
available_measures <- function() {
  # Define available measurements
  # These correspond to get_* functions that return scalars
  measures <- c(
    # Size and perimeter
    "area",
    "perim",

    # Centroid size
    "centroid_size",
    "centroid_size_norm",

    # Circularity measures
    "circularity",
    "circularity_norm",
    "circularity_haralick",

    # Length and width
    "length",
    "width",
    "elongation",
    "rectangularity",

    # Convex hull measures
    "convexity",
    "solidity",

    # Complex measures
    "rectilinearity",
    "calliper"
  )

  measures
}


# Helper to get measurement function ----

#' Get measurement function by name
#'
#' Internal helper to retrieve the appropriate get_* function for a measurement.
#'
#' @param name Character. Name of the measurement.
#'
#' @return The corresponding get_* function.
#'
#' @keywords internal
#' @noRd
.get_measure_function <- function(name) {
  # Map measurement names to getter functions
  fn_name <- paste0("get_", name)

  # Try to get the function from package namespace or global env
  fn <- tryCatch(
    get(fn_name, mode = "function"),
    error = function(e) NULL
  )

  if (is.null(fn)) {
    available <- available_measures()
    stop(sprintf(
      "Unknown measurement: '%s'\nAvailable measurements: %s\nUse available_measures() to see all options.",
      name, paste(available, collapse = ", ")
    ))
  }

  fn
}


# Print method for measurement results ----

#' @export
print.momocs_measures <- function(x, ...) {
  cat("# Momocs2 measurements\n")
  NextMethod()
}
