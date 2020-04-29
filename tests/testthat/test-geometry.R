test_that("ed works", {
  expect_equivalent(ed(c(0, 0), c(1, 1)), sqrt(2))

  x <- tibble(x=0:5, y=0:5) %>% coo_single()
  expect_equivalent(ed_pw(x, x+1), rep(sqrt(2), nrow(x)))

  xn <- ed_nearest(x, tibble(x=1.1, y=1.1))
  expect_equivalent(xn$d, sqrt(2)/10)
  expect_equivalent(xn$ids, 2)

  xf <- ed_furthest(x, tibble(x=50, y=50))
  expect_equivalent(xf$d, 50*sqrt(2))
  expect_equivalent(xf$ids, 1)

  xc <- ed_calliper(x)
  expect_equivalent(xc$d, sqrt(2)*5)
  expect_equivalent(xc$ids, c(1, 6))

  x <- tibble(x=c(0, 1, 0.45, 1, 0), y=c(0, 0, 0.45, 1, 1)) %>% coo_single()
  expect_equivalent(ed_minrad(x)$ids, 3)

})
