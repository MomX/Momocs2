
#' Extract landmark coordinates
#'
#' From a [mom_tbl] with coordinates (eg outlines or curves) _and_
#' a landmark column (a list of row ids being landmarks for each shape).
#'
#' @param x [mom_tbl]
#' @param from_col column name (`coo` by default)
#' @param ldk_col column name (`ldk` by default)
#'
#' @family ldk with a new column named {{from_col}}_{{ldk_col}}.
#' @examples
#' h <- hearts %>% get_ldk()
#' h$coo_ldk %>% head()
#' @export
get_ldk <- function(x, from_col=coo, ldk_col=ldk){
  UseMethod("get_ldk")
}

#' @export
get_ldk.default <- function(x, from_col=coo, ldk_col=ldk){
  not_defined("get_ldk")
}

#' @export
get_ldk.mom_tbl <- function(x, from_col=coo, ldk_col=ldk){

  # tidyeval
  c(from_col, ldk_col) %<-% tidyeval_coo_and_ldk({{from_col}}, {{ldk_col}})

  # operate
  purrr::map2(.x = dplyr::pull(x, !!from_col),
              .y = dplyr::pull(x, !!ldk_col),
              .f = ~.x[.y, ]) -> res
  dplyr::mutate(x, "{{from_col}}_{{ldk_col}}" := coo_list(res))
}
