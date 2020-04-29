test_that("coeff_split works", {
  x <- runif(48)
  xs <- .coeff_split(x)
  expect_true(is.list(xs))
  expect_identical(names(xs), c("an", "bn", "cn", "dn"))

  expect_is(bot %>% pick(1) %>% efourier() %>% .coeff_split(), "list")
})

test_that(" .check_efourier_nb_h works", {

  # 6 is supposed to pass; Inf is not
  # coo_single
  expect_equal(bot %>% pick() %>% .check_efourier_nb_h(6), 6)
  expect_message(x <- bot %>% pick() %>% .check_efourier_nb_h(Inf))
  expect_is(x, "numeric")

  # coo_list
  expect_equal(bot$coo %>% pick() %>% .check_efourier_nb_h(6), 6)
  expect_message(x <- bot$coo %>% pick() %>% .check_efourier_nb_h(Inf))
  expect_is(x, "numeric")

  # mom_tbl
  expect_equal(bot %>% pick() %>% .check_efourier_nb_h(6), 6)
  expect_message(x <- bot %>% pick() %>% .check_efourier_nb_h(Inf))
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

  # mom_tbl now
  expect_s3_class(bot %>% efourier(), "mom_tbl")

  # drop or not
  kept    <- bot %>% efourier(keep=TRUE) %>% class()
  dropped <- bot %>% efourier(keep=FALSE) %>% class()

  expect_true(kept[1]=="mom_tbl")
  expect_true(dropped[1]=="mom_tbl")

  # picking nb_h message
  expect_message(x <- bot %>% efourier())
  expect_is(x, "mom_tbl")

  # too ambitious nb_h
  expect_message(x <- bot %>% efourier(nb_h=1e5))
  expect_is(x, "mom_tbl")
})

test_that("efourier_i works", {
  x <- bot %>% pick(1) %>% efourier
  y <- x %>% efourier_i(nb_pts = 1e4)
  expect_is(y, "coo_single")
  expect_true(nrow(y)==1e4)

  # when arrives as numeric
  z <- y %>% unlist() %>% efourier_i(nb_pts = 12)
  expect_is(z, "coo_single")
  expect_true(nrow(z)==12)

  # no nb_pts, no nb_h
  x <- bot %>% pick(1) %>% efourier %>% efourier_i()
  expect_is(x, "coo_single")

  # coe_list
  x <- bot %>% efourier(4) %$% coe
  y <- x %>% efourier_i()
  expect_is(y, "coo_list")

  expect_is(bot %>% efourier(4) %>% efourier_i() %$% coe_i, "coo_list")
})

test_that("efourier_norm works",{
  x0 <- bot %>% pick(5) %>% efourier(4)
  x1 <- x0 %>% efourier_norm(start=T)
  expect_is(x1, "coe_single")
  expect_equivalent(abs(x1$a1), 1) # why not1 though
  expect_equivalent(abs(x1$b1), 0) # why not1 though
  expect_equivalent(abs(x1$c1), 0) # why not1 though

  x <- bot$coo %>% efourier(4) %>% efourier_norm()
  expect_is(x, "coe_list")
  y <- x %>% as.list %>% dplyr::bind_rows()
  expect_equivalent(abs(y$a1), rep(1, nrow(y)))
  expect_equivalent(abs(y$b1), rep(0, nrow(y)))
  expect_equivalent(abs(y$c1), rep(0, nrow(y)))
})



