test_that("coeff_split works", {
  x <- runif(48)
  xs <- .coeff_split(x)
  expect_true(is.list(xs))
  expect_identical(names(xs), c("an", "bn", "cn", "dn"))
})

test_that("efourier works", {
  # if not a coo or Coo
  expect_message(efourier("a"))
  # missing nb_h
  x <- matrix(runif(24), ncol=2) %>% coo_single()
  expect_message(efourier(x))
  expect_message(efourier(x, 1e5))
  expect_true(efourier(x) %>% is.list())
  # same length
  expect_equal(efourier(x)[1:4] %>% purrr::map_dbl(length) %>% unique() %>% length(), 1)
  # right components, correctly named
  expect_true(efourier(x) %>% names() %in% c("a", "b", "c", "d", "a0", "c0") %>% all)

  expect_s3_class(efourier(x) %>% as_tibble, "tbl_df")

  # coo_tbl now
  expect_s3_class(bot2 %>% efourier(), "coe_tbl")

  # drop or not
  kept    <- bot2 %>% efourier(keep=TRUE) %>% class()
  dropped <- bot2 %>% efourier(keep=FALSE) %>% class()

  expect_identical(kept[1:2], c("coe_tbl", "coo_tbl"))
  expect_true(dropped[1]==c("coe_tbl"))
  expect_false(dropped[2]==c("coo_tbl"))
})



