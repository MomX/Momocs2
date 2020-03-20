# pick ----------------------------------------------------
#' Pick a single coo_df from a coo_tbl
#'
#' @param x a `coo_tbl` or a `coo_list`
#' @param i `int` which to extract. If not provided, pick one randomly.
#' @return a coo_df
#' @examples
#' bot2 %>% pick(1) %>% gg()
#' bot2$coo %>% pick(2)
#' @export
pick <- function(x, i){
  UseMethod("pick")
}

#' @export
pick.default <- function(x, i){
  .msg_warning("pick: do not know how to pick on this class")
}

# #' @export
# pick.list <- function(x, i){
#   if (missing(i))
#     i <- sample(length(x), 1)
#   x[[i]]
# }

#' @export
pick.coo_tbl <- function(x, i){
  if (missing(i))
    i <- sample(nrow(x), 1)
  x$coo[[i]]
}

#' @export
pick.coo_list <- function(x, i){
  if (missing(i))
    i <- sample(length(x), 1)
  x[[i]]
}

# plint ---------------------------------------------------
#' Plot, print and pipe forward
#'
#' Within a pipe, will take left hand side and:
#' i)  produce the default graphics (using [gg]),
#' ii) print the object  (using [print]) and
#' iii) forward in untouched to right hand side of the pipe.
#' @param x object to plint
#' @param ... additional parameters
#'
#' @return what was passed to it (`x`), untouched. Side effects are plotting and printing
#'
#' @export
plint <- function(x, ...){
  UseMethod("plint")
}

#' @export
plint.coo_single <- function(x, ...){
  x %>% gg(...) %>% print()
  x %>% print()
  x
}


# unpack --------------------------------------------------
#' Unpack list columns
#'
#' Unpack list columns such as [coo_list] and [coe_list], repeat lines if required. Differs from unnest,
#' in that it adds a "shp" column.
#'
#' @param x a Momocs object
#' @param ... useless
#' @examples
#'
#' bot2$coo %>% unpack
#'
#' bot2 %>% unpack
#' @export
unpack <- function(x, ...) {
  UseMethod("unpack")
}

#' @export
unpack.default <- function(x, ...){
  .msg_info("unpack: no unpack method for this class")
}

#' @export
unpack.list <- function(x, ...){
  x %>%
    purrr::imap(~.x %>%
                  dplyr::mutate(shp=.y) %>%
                  dplyr::select(shp, dplyr::everything())) %>%
    # make it a single tbl
    dplyr::bind_rows() %>%
    # remove the class
    dplyr::as_tibble()
}

#' @export
unpack.coo_tbl <- function(x, ...){
  # how many times do we have to repeat each line
  # ie number of rows for each coo_tbl
  reps <- rep(1:nrow(x), times=purrr::map_dbl(x$coo, nrow))
  # unpack coo, bind cols with others
  dplyr::bind_cols(
    x$coo %>% unpack(),
    x %>% dplyr::select(-coo) %>% dplyr::slice(reps)
  )
}


# prefix_within -------------------------------------------


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


# choose_coe()
# select_coe()
# remove_coe()


only_coe <- function(x){
  dplyr::select_if(x, is_coe_list)
}

drop_coe <- function(x){
  dplyr::select_if(x, purrr::negate(is_coe_list))
}

select_coe <- function(x, ...){
  y <- x %>% drop_coe()

  x %>% dplyr::select(!!!enquos(...)) %>%
    dplyr::bind_cols(y)
}
#
# x <- bot2 %>% efourier(4)
# x$plop <- bot2$coo %>% efourier(6)
# x$plip <- bot2$coo %>% efourier(12)
# x %>% only_coe()
# x %>% drop_coe()
# x %>% select_coe(coe, plip)
#
# x %>% select_coe(starts_with("pl"))
#
#
# bot2 %>% efourier %>% select_coe()

ncoe <- function(x) x %>% purrr::map_lgl(is_coe_list) %>% sum()
ncoo <- function(x) x %>% purrr::map_lgl(is_coo_list) %>% sum()
# bot2 %>% ncoo
# bot2 %>% efourier(4) %>% ncoe



