
test_that("dfourier works", {
  expect_message(dfourier("a"), "no method")
  x <- olea %>% pick(1)
  expect_message(dfourier(x), "not provided")
  xl <- dfourier(x, nb_h=3, raw=TRUE)
  expect_is(xl, "list")
  expect_equal(names(xl), c("an", "bn", "mod", "phi"))

  k=3
  x <- dfourier(x, nb_h=k) # raw FALSE by default
  expect_is(x, "coe_single")
  expect_is(x, "data.frame")
  expect_true(ncol(x)==k*2)
  expect_true(nrow(x)==1)

  # coo_list
  expect_error(dfourier(bot$coo[1:5], nb_h=1), "bookstein")

  # mom
  expect_error(dfourier(bot[1:2, ], nb_h=1), "bookstein")

  x <- olea[1:2, ]
  expect_message(dfourier(x, nb_h=1, raw=TRUE), "useless")
})
