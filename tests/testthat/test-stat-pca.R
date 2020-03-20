p <- bot2 %>% efourier(4) %>% stat_pca()


test_that("stat_pca works", {
  bf <- bot2 %>% efourier(4)
  expect_message(bf %>% stat_pca())
  expect_message(bf %>% stat_pca(center=T))  # hey you when you will have implemented stat_pca.defaukt
  expect_message(bf %>% stat_pca(scale=T))
  x <- bf %>% stat_pca(center=TRUE, scale=TRUE)
  expect_is(x, "tbl")
  expect_is(x, "pca")
})

test_that("scree works", {

  expect_equal(scree(p, n_axes=4) %>% nrow(), 4)
  expect_equal(max(scree(p)$cumsum), 1)

  # all variance == scree
  expect_equal(scree_min(p, 1), nrow(scree(p)))

  expect_is(scree_plot(p, 3), "gg")
  expect_is(scree_plot(p, prop=0.5), "gg")
})

