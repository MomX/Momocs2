# # COO ------
# coo_single -------

test_that("coo_single constructor works", {
  # anything that can be coerced by tibble should work
list(
    matrix(1:2, ncol=2),
    matrix(1:4, ncol=2),
    list(x=1:5, y=5:1),
    matrix(1:2, ncol=2) %>% `colnames<-`(c("x", "y")) %>% as.data.frame(),
    matrix(1:2, ncol=2) %>% as.data.frame(),
    tibble::tibble(x=1:3, y=3:1),
    tibble::tibble(x=1:3, y=3:1) %>% coo_single(),
    tibble::tibble(x=1:3, y=3:1) %>% `colnames<-`(c("X", "Y"))
  ) %>% purrr::walk(~ .x %>% coo_single %>% is_coo_single %>% expect_true())

  expect_message("a" %>% coo_single(), "use coo_single()")
  expect_message(list(x="a") %>% coo_single(), "use coo_single()")
  expect_message(1:2 %>% coo_single(), "use coo_single()")
  expect_message(matrix(c(NA, 2), ncol=2) %>% coo_single(), "use coo_single()")
  expect_message(matrix(c(NA, 2:4), ncol=2) %>% coo_single(), "use coo_single()")
  expect_message(matrix(c(NA, 2), ncol=2) %>% `colnames<-`(c("x", "y")) %>% as.data.frame()%>% coo_single(), "use coo_single()")
  expect_message( matrix(c(NA, 2), ncol=2) %>% as.data.frame() %>% coo_single(), "use coo_single()")
  # expect_message(tibble::tibble(x=c(NA, 2:3), y=3:1) %>% coo_single(), "use coo_single()")
  expect_message(tibble::tibble(x=c(NA, 2:3), y=3:1) %>% coo_single(), "use coo_single()")
  expect_message(tibble::tibble(x=c(NA, 2:3), y=3:1) %>% coo_single(), "use coo_single()")
})
#
# # coo_list -------
test_that("coo_list works", {
  expect_output(bot$coo %>% print)
  expect_message(coo_list("a"))
  # list of matrices
  x <- purrr::map(bot$coo[1:5], ~.x %>% as.matrix) %>% coo_list()
  expect_is(x, "coo_list")
  expect_true(all(purrr::map_lgl(x, .is_class1, "coo_single")))

  # single one
  expect_is(bot %>% pick(1) %>% coo_list(), "coo_list")

  # test dplyr retain coo_list
  expect_is(bot %>% dplyr::slice(1) %>% dplyr::pull(coo), "coo_list")
  expect_is(bot %>% dplyr::mutate(coo2=coo) %>% dplyr::pull(coo2), "coo_list")
})


#
#
# # mom_tbl --------
# test_that("mom_tbl works", {
#   # who are you
#   expect_message(mom_tbl("a"))
#
#   # print method
#   expect_output(print(bot))
#
#   # old Coo
#   old_Coo <- list(coo=bot$coo,
#                   fac=bot %>% dplyr::select(-coo),
#                   ldk=replicate(nrow(bot), sample(1:10, 3), simplify = FALSE)) %>%
#     .append_class("Coo") %>% .append_class("Out")
#   x <- old_Coo %>% mom_tbl()
#   expect_is(x, "mom_tbl")
#   expect_true(all(c("coo", "ldk", "type") %in% colnames(x)))
#
#   # list
#   expect_is(bot$coo %>% mom_tbl(), "mom_tbl")
#
#   expect_is(bot %>% pick(1) %>% mom_tbl(), "mom_tbl")
# })
#
# # COE -----------
# # coe_single ----
#
# test_that("coe_single works", {
#   expect_message(coe_single("foo"))
#
#   # passing a tibble
#   x <- tibble::tibble(a=1, b=2)
#   expect_is(coe_single(x), "coe_single")
#
#   # passing coe_single
#   expect_identical(coe_single(x), coe_single(coe_single(x)))
#
#   # passing a numeric
#   expect_is(coe_single(c(4, 5, 6)), "coe_single")
#
#   y <- list(tibble::tibble(a=3, b=4),
#             x)
#   expect_is(coe_list(y), "coe_list")
#   expect_message(coe_list("foo"))
# })
#
# # coe_list ----
# test_that("coe_list works", {
#   expect_message(coe_list("foo"))
#
#   x <- bot %>%
#     dplyr::slice(1:4) %$% coo %>%
#     purrr::map(efourier, nb_h=3)
#   expect_is(coe_list(x), "coe_list")
# })
#
# # coe_tbl ----
# test_that("coe_tbl works", {
#   bot %>% efourier(2) -> x
#   expect_is(x, "coe_tbl")
# })
#
# # validate_coe_single ----
# test_that("validate_coe_single validator works",{
#   x_ok <- coe_single(c(var1=1, var2=2))
#
#   # a valid one
#   expect_is(x_ok %>% validate_coe_single(), "coe_single")
#
#   # drop tbl class
#   y <- x_ok
#   class(y) <- c("coe_single", "data.frame")
#   expect_message(validate_coe_single(y))
#
#   # wrong colnames
#   # expect_message(x_ok %>% `colnames<-`(c("a", "y")) %>% validate_coo_single())
#
#   # no column
#   expect_message(x_ok %>% dplyr::select() %>% validate_coe_single())
#
#   # more than one column
#   # might be release when nrow()>1 will be allowed
#   expect_message(dplyr::bind_rows(x_ok, x_ok) %>% validate_coe_single())
# })
#
