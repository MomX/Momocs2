test_that("coeff_split works", {
  x <- runif(48)
  xs <- .coeff_split(x)
  expect_true(is.list(xs))
  expect_identical(names(xs), c("an", "bn", "cn", "dn"))
})

test_that(" .check_efourier_nb_h works", {

  # 6 is supposed to pass; Inf is not
  # coo_single
  expect_equal(bot2 %>% pick() %>% .check_efourier_nb_h(6), 6)
  expect_message(x <- bot2 %>% pick() %>% .check_efourier_nb_h(Inf))
  expect_is(x, "numeric")

  # coo_list
  expect_equal(bot2$coo %>% pick() %>% .check_efourier_nb_h(6), 6)
  expect_message(x <- bot2$coo %>% pick() %>% .check_efourier_nb_h(Inf))
  expect_is(x, "numeric")

  # coo_tbl
  expect_equal(bot2 %>% pick() %>% .check_efourier_nb_h(6), 6)
  expect_message(x <- bot2 %>% pick() %>% .check_efourier_nb_h(Inf))
  expect_is(x, "numeric")
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
  # when raw right components, correctly named
  expect_true(efourier(x, raw = TRUE) %>% names() %in% c("a", "b", "c", "d", "a0", "c0") %>% all)

  # when raw right components, correctly named
  expect_true(efourier(x, raw = FALSE) %>% is_coe_single())
  expect_equal(efourier(x, raw = FALSE) %>% class() %>% `[`(1), "efourier_single")

  expect_s3_class(efourier(x) %>% as_tibble, "tbl_df")

  # coo_tbl now
  expect_s3_class(bot2 %>% efourier(), "coe_tbl")

  # drop or not
  kept    <- bot2 %>% efourier(keep=TRUE) %>% class()
  dropped <- bot2 %>% efourier(keep=FALSE) %>% class()

  expect_identical(kept[1:2], c("coe_tbl", "coo_tbl"))
  expect_true(dropped[1]==c("coe_tbl"))
  expect_false(dropped[2]==c("coo_tbl"))

  # picking nb_h message
  expect_message(x <- bot2 %>% efourier())
  expect_is(x, "coe_tbl")

  # too ambitious nb_h
  expect_message(x <- bot2 %>% efourier(nb_h=1e5))
  expect_is(x, "coe_tbl")
})

test_that("efourier_i works", {
  x <- bot2 %>% pick(1) %>% efourier %>% efourier_i(nb_pts = 1e4)
  expect_is(x, "coo_single")
  expect_true(nrow(x)==1e4)

  # no nb_pts, no nb_h
  x <- bot2 %>% pick(1) %>% efourier %>% efourier_i()
  expect_is(x, "coo_single")
})



