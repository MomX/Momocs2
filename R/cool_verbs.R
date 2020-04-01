
# prefix_within -------------------------------------------


# list_columns verbs --------------------------------------
#' List columns verbs
#'
#' @param x [coo_tbl] or a [coe_tbl]
#' @param ... additional parameters, just like in `dplyr::select`
#' @name list_columns
NULL

#' @export
#' @describeIn list_columns count the number of coo columns
coo_nb <- function(x){
  x %>% purrr::map_lgl(is_coo_list) %>% sum()
}

#' @export
#' @describeIn list_columns count the number of coe columns
coe_nb <- function(x){
  x %>% purrr::map_lgl(is_coe_list) %>% sum()
}

#' @export
#' @describeIn list_columns only retain coo columns
coo_only <- function(x){
  dplyr::select_if(x, is_coo_list)
}

#' @export
#' @describeIn list_columns only retain coe columns
coe_only <- function(x){
  dplyr::select_if(x, is_coe_list)
}

#' @export
#' @describeIn list_columns drop coo columns
coo_drop <- function(x){
  dplyr::select_if(x, purrr::negate(is_coo_list))
}

#' @export
#' @describeIn list_columns drop coe columns
coe_drop <- function(x){
  dplyr::select_if(x, purrr::negate(is_coe_list))
}

#' @export
#' @describeIn list_columns use select among coo columns
coo_select <- function(x, ...){
  y <- x %>% coo_drop()
  x %>% dplyr::select(!!!enquos(...)) %>%
    dplyr::bind_cols(y)
}

#' @export
#' @describeIn list_columns use select among coe columns
coe_select <- function(x, ...){
  y <- x %>% coe_drop()
  x %>% dplyr::select(!!!enquos(...)) %>%
    dplyr::bind_cols(y)
}


# Prefix col ----------------------------------------------
# todo prefix within
# Given coe_tbl with 1 or mroe coe_list columns
# Prefix individual coe_tbl colnames composing them with coe_list column name
# See examples
prefix_col <- function(x){
  a <- x %>% dplyr::select_if(is_coe_list)
  b <- x %>% dplyr::select_if(is_coe_list %>% purrr::negate())
  new_a <- purrr::imap(a,
                       ~.x %>% dplyr::bind_rows() %>%
                         `colnames<-`(paste0(.y, "_", colnames(.))) %>%
                         split(., 1:nrow(.)) %>%
                         `names<-`(names(.x))) %>%
    tibble::as_tibble()
  dplyr::bind_cols(new_a, b)
}

