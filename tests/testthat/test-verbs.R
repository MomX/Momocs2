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

test_that("unfold works", {
  expect_message(unfold("a"))

  x <- bot2 %>% unfold()
  expect_false(is(x, "coo_tbl"))
  expect_false(is(x, "coo_list"))
  # also tests for 3 columns
  expect_true(all(colnames(x)[1:3] == c("shp", "x", "y")))

  # and that with NULL names, unfold still works
  y <- bot2
  names(y$coo) <- NULL
  x <- y %>% unfold
  expect_false(is(x, "coo_tbl"))
  expect_false(is(x, "coo_list"))
  # also tests for 3 columns
  expect_true(all(c("shp", "x", "y") %in% colnames(x)))
  expect_true(is.numeric(x$shp))
})
