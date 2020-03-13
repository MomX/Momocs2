# coo_single -----------------------------------------

# combined constructor, validator and coercers

#' coo_single constructor for single shapes
#'
#' `coo` objects are [tibble][tibble::tibble-package] with _exactly_ two columns named `x` and `y`
#' @rdname coo_single
#' @param x anything that can be turned into a coo_single, typically something that lloks like a
#'  two columns
#'  @details For `tibble` and `data.frame`, the first two columns are selected.
#' @return a `coo_single` object
#' @export
#' @examples
#' matrix(1:12, ncol=2) %>% coo_single()
coo_single <- function(x) {
  UseMethod("coo_single")
}

#' @export
coo_single.default <- function(x){
  .msg_warning("do not know how to turn into a coo_single")
}

#' @export
coo_single.matrix <- function(x){
  x[, 1:2] %>% `colnames<-`(c("x", "y")) %>%
    tibble::as_tibble() %>% .append_class("coo_single")
}

#' @export
coo_single.array <- function(x){
  stopifnot(length(dim(x))==3)
  x[,,1] %>% coo_single()
}

#' @export
coo_single.data.frame <- function(x){
  x %>% dplyr::select(1:2) %>% .append_class("coo_single")
}

#' @export
coo_single.coo_single <- function(x){
  x %>% dplyr::select(.data$x, .data$y) %>% .append_class("coo_single")
}


#' @export
#' @describeIn coo_single Class tester
is_coo_single <- function(x){
  x %>% .is_class1("coo_single")
}

#' @export
#' @describeIn coo_single Class validator
validate_coo_single <- function(x){
  res <- list()
  if (!is_coo_single(x))
    res <- append(res, "must be a <coo_single>")

  if (!tibble::is_tibble(x))
    res <- append(res, "must be a <tbl>")

  if (!is.data.frame(x))
    res <- append(res, "must be a <data.frame>")

  if (!ncol(x)==2)
    res <- append(res, "must be two columns")

  if (!identical(colnames(x), c("x", "y")))
    res <- append(res, "columns must be named 'x' and 'y'")

  if (sum(is.na(x))>0)
    res <- append(res, "must not have NAs")

  if (nrow(x)<3)
    res <- append(res, "must have more than two points")

  if (length(res)>0){
    purrr::walk(res, cli::cli_alert_danger)
    .check(FALSE, "use coo_single()")
  }
  return(x)
}

# coo0 <- function(nrow=10){
#   matrix(stats::runif(nrow*2), ncol=2) %>% coo()
# }

#' @export
plot.coo_single <- function(x, ...){
  gg(x, ...)
}

# print ---
#' @export
print.coo_single <- function(x, ...){
  x %>% tibble::as_tibble() %>% print(...)
  glue::glue(cli::symbol$pointer, " a ",
             crayon::bgBlue("coo_single"), " ",
             "with {nrow(x)} points") %>%
    cli::cat_line()
  # just as print.default
  invisible(x)
}



#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.coo_single <- function(x, ...) {
  out <- format(x)
  out[is.na(x)] <- NA
  pillar::new_pillar_shaft_simple(out, align = "center")
}

#' @importFrom pillar type_sum
#' @export
type_sum.coo_single <- function(x) {
  "coo_single"
}

# format.coo <- function(x, ...){
#   x[1, ] %>% as.numeric() %>% paste(collapse="; ")
# }


# # #
# replicate(10, matrix(stats::runif(100), ncol=2), simplify=FALSE) %>% Coo()
# # x


# coo_list ------------------------------------------------
# combined constructor, validator and coercers

#' coo_list constructor for list of shapes
#'
#' `coo_list` objects are [list] of [coo_single].
#' @rdname coo_list
#' @param x anything that can be turned into a `coo_list`, typically a list of [coo_single]
#' @details They behave like lists
#' and thus as regular [tibble]/[data.frame] columns.
#' The only difference is that and are understood by morphometric methods used in MomX.
#' Front users are not likely to use them directly.
#' @return a `coo_list` object
#' @examples
#' matrix(1:12, ncol=2) %>% list() %>% coo_list()
#' @export
coo_list <- function(x) {
  UseMethod("coo_list")
}

#' @export
coo_list.default <- function(x){
  .msg_warning("do not know how to turn into a coo_list")
}

#' @export
coo_list.list <- function(x){
  x %>%
    purrr::modify_if(purrr::negate(is_coo_single), coo_single) %>%
    .append_class("coo_list")
}

# print ---
#' @export
print.coo_list <- function(x, ...){
  x %>% print.default()
  glue::glue(cli::symbol$pointer, " a ",
             crayon::bgBlue("coo_list"), " ",
             "with {length(x)} ", crayon::bgBlue("coo_single")) %>%
    cli::cat_line()
  invisible(x)
}

# coo_tbl -------------------------------------------------
#' coo_tbl class
#'
#' @rdname coo_tbl
#' @param x anything sensible
#' @export
coo_tbl <- function(x){
  UseMethod("coo_tbl")
}

#' @export
coo_tbl.default <- function(x){
  .msg_warning("do not know how to turn into a coo_tbl")
}

#' @export
coo_tbl.coo_list <- function(x){
  tibble::tibble(coo=x) %>%
    .append_class("coo_tbl")
}

#' @export
coo_tbl.list <- function(x){
  tibble::tibble(coo=coo_list(x)) %>%
    .append_class("coo_tbl")
}

# Momocs retrocompatibility
#' @export
coo_tbl.Coo <- function(x){
  res <- dplyr::bind_cols(
    coo_tbl(x$coo),
    tibble::as_tibble(x$fac)
  ) %>% .append_class("coo_tbl")

  # include $ldk if any
  if (!is.null(x[["ldk"]]) && length(x[["ldk"]])==length(x$coo))
    res <- res %>%
      dplyr::mutate(ldk=x[["ldk"]]) %>%
      dplyr::select(coo, ldk, dplyr::everything())

  # return this beauty
  res
}


# print ---
#' @export
print.coo_tbl<- function(x, ...){
  x %>% tibble::as_tibble() %>% print(...)
  glue::glue(cli::symbol$pointer, " a ",
             crayon::bgBlue("coo_tbl"), " ",
             "with {nrow(x)} coo_single") %>%
    cli::cat_line()
  # just as print.default
  invisible(x)
}




