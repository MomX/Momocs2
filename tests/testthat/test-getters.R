test_that("getters works", {
  x <- bot2 %>% get_centpos()
  expect_equal(sum(c("centpos_x", "centpos_y") %in% colnames(x)), 2)
  expect_is(x, "coo_tbl")

  # some are coo_scale and coo_center side
  x <- bot2 %>% get_centsize()
  expect_is(x, "coo_tbl")
  y <- bot2$coo %>% get_centsize
  # tests equality and column presence
  expect_true(all(x$centsize==y))
})

