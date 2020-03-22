test_that("pick works", {
  p1 <- bot2 %>% pick(1)
  p1_bis <- bot2$coo[[1]]
  p1_ter <- dplyr::slice(bot2, 1)$coo[[1]]

  expect_identical(p1, p1_bis)
  expect_identical(p1, p1_ter)

  expect_message(pick("a"))
  # id specified or not
  expect_is(bot2$coo %>% pick(1), "coo_single")
  expect_is(bot2$coo %>% pick(), "coo_single")
})

test_that("plint works", {
  x <- matrix(1:12, ncol=2) %>% coo_single()
  # with no rhs
  expect_output(plint(x))
  expect_s3_class(plint(x), "coo_single")
})

test_that("unpack works", {
  expect_message(unpack("a"))

  x <- bot2$coo %>% unpack()
  expect_false(is(x, "coo_tbl"))
  expect_false(is(x, "coo_list"))
  # also tests for 3 columns
  expect_true(all(colnames(x)[1:3] == c("shp", "x", "y")))

  x <- bot2 %>% unpack
  expect_false(is(x, "coo_tbl"))
  expect_false(is(x, "coo_list"))
  # also tests for 3 columns
  expect_true(all(colnames(x)[1:3] == c("shp", "x", "y")))
  # tests for names propagation
  expect_true(is.character(x$shp))

  # and that with NULL names, unpack still works
  y <- bot2
  names(y$coo) <- NULL
  x <- y %>% unpack
  expect_false(is(x, "coo_tbl"))
  expect_false(is(x, "coo_list"))
  # also tests for 3 columns
  expect_true(all(c("shp", "x", "y") %in% colnames(x)))
  expect_true(is.numeric(x$shp))
})

# coe verbs ---------
test_that("coe verbs work",{

  x <- bot2 %>% efourier(4)
  x$coe2 <- bot2$coo %>% efourier(6)

  expect_equal(x %>% select_coe(coe) %>% only_coe() %>% colnames(), "coe")
  expect_equal(x %>% select_coe(-coe) %>% only_coe() %>% colnames(), "coe2")

  expect_equal(x %>% only_coe() %>% colnames(), c("coe", "coe2"))
  expect_equal(x %>% drop_coe() %>% colnames(), bot2 %>% dplyr::select(-coo) %>% colnames())

})



