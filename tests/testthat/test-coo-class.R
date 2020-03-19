# coo_single -------

test_that("coo_single constructor works", {
  # single line matrix
  m <- matrix(1:2, ncol=2)
  expect_true(m %>% coo_single() %>% is_coo_single())


  m <- matrix(1:6, ncol=2)
  expect_true(m %>% coo_single() %>% is_coo_single())


  a <- array(1:24, dim=c(6, 2, 2))
  expect_true(a %>% coo_single %>% is_coo_single())

  df <- iris
  expect_true(df %>% coo_single %>% is_coo_single())

  expect_true(m %>% coo_single %>% coo_single %>% coo_single %>% is_coo_single())

  expect_message("a" %>% coo_single(), "do not know")
  expect_message(list("a") %>% coo_single, "do not know")
  expect_message(1:5 %>% coo_single, "do not know")
})

test_that("coo_single validator works",{
  x_ok <- coo_single(matrix(1:12, ncol=2))

  # a valid one
  expect_s3_class(x_ok %>% validate_coo_single(), "coo_single")

  # wrong colnames
  # expect_message(x_ok %>% `colnames<-`(c("a", "y")) %>% validate_coo_single())

  # single column
  expect_message(x_ok %>% dplyr::select(1) %>% validate_coo_single())

  # three columns
  expect_message(x_ok %>% dplyr::mutate(foo=999) %>% validate_coo_single())

  # not df
  expect_message(x_ok %>% as.matrix() %>% validate_coo_single())

  # not tbl
  expect_message(x_ok %>% as.data.frame() %>% validate_coo_single())

  # not coo_tbl
  expect_message(x_ok %>% tibble::as_tibble() %>% validate_coo_single())

  # less than 3 points
  expect_message(x_ok %>% dplyr::slice(1:2) %>% validate_coo_single())

  # with some NAs
  x_with_NA <- x_ok
  x_with_NA[1, 2] <- NA
  expect_message(x_with_NA  %>% validate_coo_single())
})

# pillar
test_that("pillar works", {
  expect_is(pillar::pillar_shaft(bot2 %>% pick(5) %>% list()), "pillar_shaft_simple")
})


# coo_list -------
test_that("coo_list works", {
  expect_output(bot2$coo %>% print)
  expect_message(coo_list("a"))
  # list of matrices
  x <- purrr::map(bot2$coo[1:5], ~.x %>% as.matrix) %>% coo_list()
  expect_is(x, "coo_list")
  expect_true(all(purrr::map_lgl(x, .is_class1, "coo_single")))

  # single one
  expect_is(bot2 %>% pick(1) %>% coo_list(), "coo_list")

})


# coo_tbl --------
test_that("coo_tbl works", {
  # who are you
  expect_message(coo_tbl("a"))

  # print method
  expect_output(print(bot2))

  # old Coo
  old_Coo <- list(coo=bot2$coo,
                  fac=bot2 %>% dplyr::select(-coo),
                  ldk=replicate(nrow(bot2), sample(1:10, 3), simplify = FALSE)) %>%
    .append_class("Coo") %>% .append_class("Out")
  x <- old_Coo %>% coo_tbl()
  expect_is(x, "coo_tbl")
  expect_true(all(c("coo", "ldk", "type") %in% colnames(x)))

  # list
  expect_is(bot2$coo %>% coo_tbl(), "coo_tbl")

  expect_is(bot2 %>% pick(1) %>% coo_tbl(), "coo_tbl")
})

test_that("coe_tbl works", {
  bot2 %>% efourier(2) -> x
  expect_is(x, "coe_tbl")
})

