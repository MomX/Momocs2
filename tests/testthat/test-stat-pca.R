test_that("scree works", {
  p <- bot2 %>% efourier(4) %>% stat_pca()
  expect_equal(scree(p, n_axes=4) %>% nrow(), 4)
  expect_equal(max(scree(p)$cumsum), 1)

  # all variance == scree
  expect_equal(scree_min(p, 1), nrow(scree(p)))

  expect_is(scree_plot(p, 3), "gg")
  expect_is(scree_plot(p, prop=0.5), "gg")
})
