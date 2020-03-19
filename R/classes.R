# COO -----------------------------------------------------
# coo_single ----------------------------------------------

# combined constructor, validator and coercers

#' coo_single constructor for single shapes
#'
#' `coo` objects are [tibble][tibble::tibble-package] with _exactly_ two columns named `x` and `y`
#' @rdname coo_single
#' @param x anything that can be turned into a coo_single, typically something that looks like a
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
  .msg_warning("coo_single: do not know how to turn into a coo_single")
}

#' @export
coo_single.matrix <- function(x){
  # drop=FALSE to prevent matrix dropping when single line
  x[, 1:2, drop=FALSE] %>% `colnames<-`(c("x", "y")) %>%
    tibble::as_tibble() %>% .append_class("coo_single")
}

#' @export
coo_single.array <- function(x){
  stopifnot(length(dim(x))==3)
  x[,,1] %>% coo_single()
}

#' @export
coo_single.data.frame <- function(x){
  x %>%
    dplyr::select(1:2) %>%
    `colnames<-`(c("x", "y")) %>%
    tibble::as_tibble() %>%
    .append_class("coo_single")
}

#' @export
coo_single.coo_single <- function(x){
  x %>%
    dplyr::select(.data$x, .data$y) %>%
    .append_class("coo_single")
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

  if (ncol(x)!=2)
    res <- append(res, "must be two columns")

  # if (!identical(colnames(x), c("x", "y")))
  #   res <- append(res, "columns must be named 'x' and 'y'")

  if (sum(is.na(x))>0)
    res <- append(res, "must not have NAs")

  if (nrow(x)<3)
    res <- append(res, "must have more than two points")

  if (length(res)>0){
    purrr::walk(res, cli::cli_alert_danger)
    .msg_warning("use coo_single()")
  } else {
    x
  }
}

# coo0 <- function(nrow=10){
#   matrix(stats::runif(nrow*2), ncol=2) %>% coo()
# }

#' @export
plot.coo_single <- function(x, ...){
  gg(x, ...)
}

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
  .msg_warning("coo_list: do not know how to turn into a coo_list")
}

#' @export
coo_list.coo_single <- function(x){
  x %>% list() %>% .append_class("coo_list")
}

#' @export
coo_list.list <- function(x){
  x %>%
    purrr::modify_if(purrr::negate(is_coo_single), coo_single) %>%
    .append_class("coo_list")
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
  .msg_warning("coo_tbl: do not know how to turn into a coo_tbl")
}

#' @export
coo_tbl.coo_single <- function(x){
  x %>% list() %>% coo_tbl()
}

#' @export
coo_tbl.coo_list <- function(x){
  tibble::tibble(coo=x) %>%
    .append_class("coo_tbl")
}

#' @export
coo_tbl.data.frame <- function(x){
  x %>%
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

# coo_printers --------------------------------------------
# pillar ---

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

#' @importFrom pillar is_vector_s3
#' @export
is_vector_s3.coo_list <- function(x) TRUE
#
# # #' @export
#' @importFrom pillar type_sum
type_sum.coo_list <- function(x) {
  "coo_list"
}


# print ---
#' @export
print.coo_single <- function(x, ...){
  x %>%
    tibble::as_tibble() %>%
    print(...) # to use base tbl print method
  glue::glue(cli::symbol$pointer, " a ",
             crayon::bgBlue("coo_single"), " ",
             "with {nrow(x)} points") %>%
    cli::cat_line()
  # just as print.default
  invisible(x)
}

#' @export
print.coo_list <- function(x, ...){
  # x[[sample(length(x), 1)]] %>% print()
  # cli::cat_line("...")
  glue::glue(cli::symbol$pointer, " a ",
             crayon::bgBlue("coo_list"), " ",
             "with {length(x)} ", crayon::bgBlue("coo_single")) %>%
    cli::cat_line()
  invisible(x)
}

#' @export
print.coo_tbl<- function(x, ...){
  x %>%
    tibble::as_tibble() %>%
    print(...)
  glue::glue(cli::symbol$pointer, " a ",
             crayon::bgBlue("coo_tbl"), " ",
             "with {nrow(x)} coo_single") %>%
    cli::cat_line()
  # just as print.default
  invisible(x)
}

# coo_testers -----------

#' @export
#' @describeIn coo_single Class tester
is_coo_single <- function(x){
  x %>% .is_class("coo_single")
}

#' @export
#' @describeIn coo_single Class tester
is_coo_single1 <- function(x){
  x %>% .is_class1("coo_single")
}


#' @export
#' @describeIn coo_list Class tester
is_coo_list <- function(x){
  x %>% .is_class("coo_list")
}

#' @export
#' @describeIn coo_list Class tester
is_coo_list1 <- function(x){
  x %>% .is_class1("coo_list")
}

#' @export
#' @describeIn coo_tbl Class tester
is_coo_tbl <- function(x){
  x %>% .is_class("coo_tbl")
}

#' @export
#' @describeIn coo_tbl Class tester
is_coo_tbl1 <- function(x){
  x %>% .is_class1("coo_tbl")
}

# COE -----------------------------------------------------
# coe_single ----------------------------------------------
# combined constructor, validator and coercers

#' coe_single constructor for single coefficients lists
#'
#' `coe` objects are [tibble][tibble::tibble-package] with as many rows as created by morphometric methods.
#' It is not really intended to be used directly but is useful if you want to extend Momocs.
#' @rdname coe_single
#' @param x anything that can be turned into a coe_single, typically something that looks like a
#'  two columns
#' @return a `coe_single` object
#' @export
#' @examples
#' tibble::tibble(a=1, b=2) %>% coe_single()
coe_single <- function(x) {
  UseMethod("coe_single")
}

#' @rdname coe_single
#' @export
coe_single.default <- function(x){
  .msg_warning("coe_single: do not know how to turn into a coe_single")
}

# #' @export
# coe_single.matrix <- function(x){
#   x[, 1, drop=FALSE] %>%
#     tibble::as_tibble() %>%
#     .append_class("coe_single")
# }

# #' @export
# coe_single.array <- function(x){
#   stopifnot(length(dim(x))==3)
#   x[,,1, drop=FALSE] %>% coe_single()
# }

#' @rdname coe_single
#' @export
coe_single.numeric <- function(x){
  if (nrow(x)>1)
    .msg_danger("coe_single: more than one row, only retain the first one")
  x %>% tibble::as_tibble() %>% .append_class("coe_single")
}

#' @rdname coe_single
#' @export
coe_single.data.frame <- function(x){
  if (nrow(x)>1)
    .msg_danger("coe_single: more than one row, only retain the first one")
  x %>% tibble::as_tibble() %>% .append_class("coe_single")
}

#' @rdname coe_single
#' @export
coe_single.list <- function(x){
  x %>% tibble::as_tibble() %>% .append_class("coe_single")
}

#' @rdname coe_single
#' @export
coe_single.coe_single <- function(x){
  x
}

#' @export
#' @describeIn coe_single Class validator
validate_coe_single <- function(x){
  res <- list()
  if (!is_coe_single(x))
    res <- append(res, "must be a <coe_single>")

  if (!tibble::is_tibble(x))
    res <- append(res, "must be a <tbl>")

  if (!is.data.frame(x))
    res <- append(res, "must be a <data.frame>")

  if (!ncol(x)>0)
    res <- append(res, "must have at least one column")

  if (is.null(colnames(x)))
    res <- append(res, "columns must be named")

  if (sum(is.na(x))>0)
    res <- append(res, "must not have NAs")

  if (nrow(x)>1)
    res <- append(res, "must have a single row")

  if (length(res)>0){
    purrr::walk(res, cli::cli_alert_danger)
    .check(FALSE, "use coe_single()")
  }
  return(x)
}

# coo0 <- function(nrow=10){
#   matrix(stats::runif(nrow*2), ncol=2) %>% coo()
# }

#' @export
# plot.coo_single <- function(x, ...){
#   gg(x, ...)
# }

# print ---
#' @export
print.coe_single <- function(x, ...){
  x %>% tibble::as_tibble() %>% print(...)  # to use base tbl print method
  glue::glue(cli::symbol$pointer, " a ",
             crayon::bgCyan("coe_single"), " ",
             "with {ncol(x)} variables") %>%
    cli::cat_line()
  # just as print.default
  invisible(x)
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.coe_single <- function(x, ...) {
  out <- format(x)
  out[is.na(x)] <- NA
  pillar::new_pillar_shaft_simple(out, align = "center")
}

#' @importFrom pillar type_sum
#' @export
type_sum.coe_single <- function(x) {
  "coe_single"
}

# coe_list ------------------------------------------------
# combined constructor, validator and coercers

#' coe_list constructor for list of shapes
#'
#' `coe_list` objects are [list] of [coe_single].
#' @rdname coe_list
#' @param x anything that can be turned into a `coe_list`, typically a list of [coe_single]
#' @details They behave like lists
#' and thus as regular [tibble]/[data.frame] columns.
#' The only difference is that and are understood by morphometric methods used in MomX.
#' Front users are not likely to use them directly.
#' @return a `coe_list` object
#' @examples
#' bot2$coe %>% efourier %>% class()
#' @export
coe_list <- function(x) {
  UseMethod("coe_list")
}

#' @export
coe_list.default <- function(x){
  .msg_warning("coe_list: do not know how to turn into a coe_list")
}

#' @export
coe_list.coe_single <- function(x){
  x %>% list() %>% .append_class("coe_list")
}

#' @export
coe_list.list <- function(x){
  x %>%
    purrr::modify_if(purrr::negate(is_coe_single), coe_single) %>%
    .append_class("coe_list")
}

# print ---
#' @export
print.coe_list <- function(x, ...){
  x %>% print.default()
  glue::glue(cli::symbol$pointer, " a ",
             crayon::bgCyan("coo_list"), " ",
             "with {length(x)} ", crayon::bgBlue("coe_single")) %>%
    cli::cat_line()
  invisible(x)
}


# coe_testers ---------------------------------------------

#' @export
#' @describeIn coe_single Class tester
is_coe_single <- function(x){
  x %>% .is_class("coe_single")
}

#' @export
#' @describeIn coe_single Class tester
is_coe_single1 <- function(x){
  x %>% .is_class1("coe_single")
}

#' @export
#' @describeIn coe_list Class tester
is_coe_list <- function(x){
  x %>% .is_class("coe_list")
}

#' @export
#' @describeIn coe_list Class tester
is_coe_list1 <- function(x){
  x %>% .is_class1("coe_list")
}

#' @export
#' @describeIn coe_tbl Class tester
is_coe_tbl <- function(x){
  x %>% .is_class("coe_tbl")
}

#' @export
#' @describeIn coe_tbl Class tester
is_coe_tbl1 <- function(x){
  x %>% .is_class1("coe_tbl")
}

# coe_tbl -------------------------------------------------
#' coe_tbl class
#'
#' @rdname coe_tbl
#' @param x anything sensible
#' @export
coe_tbl <- function(x){
  UseMethod("coe_tbl")
}

#' @export
coe_tbl.default <- function(x){
  .msg_warning("coe_tbl: do not know how to turn into a coo_tbl")
}

#' @export
coe_tbl.coe_single <- function(x){
  x %>% list() %>% coe_tbl()
}

#' @export
coe_tbl.coe_list <- function(x){
  tibble::tibble(coe=x) %>%
    .append_class("coe_tbl")
}

#' @export
coe_tbl.list <- function(x){
  tibble::tibble(coe=coe_list(x)) %>%
    .append_class("coe_tbl")
}

# Momocs retrocompatibility
#' @export
coe_tbl.Coe <- function(x){
  res <- dplyr::bind_cols(
    coe_tbl(x$coe),
    tibble::as_tibble(x$fac)
  ) %>% .append_class("coe_tbl")

  #todo: handles cuts, methods, etc.
  # # include $ldk if any
  # if (!is.null(x[["ldk"]]) && length(x[["ldk"]])==length(x$coo))
  #   res <- res %>%
  #     dplyr::mutate(ldk=x[["ldk"]]) %>%
  #     dplyr::select(coo, ldk, dplyr::everything())

  # return this beauty
  res
}
