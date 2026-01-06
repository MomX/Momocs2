#' @importFrom grDevices dev.cur dev.new
#' @importFrom stats runif var approx dist sd
#' @importFrom graphics abline axis lines par points segments text plot
#' @importFrom utils head tail
NULL

#' Pipe operators
#'
#' Momocs2 uses magrittr's pipe operators for clear, readable workflows.
#'
#' @name pipes
#' @rdname pipes
#' @aliases %>% %T>% %$%
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom magrittr %T>%
#' @importFrom magrittr %$%
#' @export %>%
#' @export %T>%
#' @export %$%
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#'
#' @details
#' **Forward pipe (`%>%`)**: Passes result forward to next function
#'
#' ```r
#' bot %>%
#'   coo_center() %>%
#'   coo_scale() %>%
#'   measure(c("area", "circularity"))
#' ```
#'
#' **Tee pipe (`%T>%`)**: Passes input forward unchanged (useful for side effects like plotting)
#'
#' ```r
#' bot %>%
#'   coo_center() %T>%
#'   pile() %>%           # Plot without breaking the chain
#'   coo_scale()
#' ```
#'
#' **Exposition pipe (`%$%`)**: Exposes column names for direct use
#'
#' ```r
#' bot %>%
#'   measure(c("area", "perim")) %$%
#'   plot(coo_area, coo_perim)  # Use columns directly without bot$
#' ```
#'
#' @seealso `magrittr` vignettes for detailed documentation.
#'
#' @examples
#' # Forward pipe - standard workflow.
#' # Almost exactly equivalent to (now native) |>
#' bot %>%
#'   coo_center() %>%
#'   coo_scale() %>%
#'   measure("area")
#'
#' # Tee pipe - plot without breaking chain
#' bot %>%
#'   coo_center() %T>%
#'   pile() %>%           # Plot without breaking the chain
#'   coo_scale()
#'
#' # Exposition pipe - access columns directly
#' bot %>%
#'   measure(c("area", "perim")) %$%
#'   plot(coo_area, coo_perim)
NULL
