test_that("likely bookstein works", {
  x0 <- tibble(x=c(-50, 50), y=c(-20, 20)) %>% coo_single()
  expect_false(likely_bookstein(x0))
  expect_true(likely_bookstein(coo_bookstein(x0)))

  x <- bot %>% pick(1) %>% coo_sample(12)
  expect_false(likely_bookstein(x))
  expect_true(likely_bookstein(coo_bookstein(x)))

})

test_that("set_names_poly works", {
  k=5
  x <- runif(k) %>% set_names_poly()
  expect_length(x, k)
  expect_equal(names(x), c("a0", "a1", "a2", "a3", "a4"))

  expect_equal(numeric(1) %>% set_names_poly() %>% names(), "a0")
})

test_that("npoly works", {
  expect_message(npoly("a"), "no method")
  x <- olea %>% pick(1)

  # degree default
  expect_message(npoly(x), "not provided")
  expect_message(npoly(olea$coo[1:2]), "not provided")
  expect_message(npoly(olea[1:2, ]), "not provided")



  expect_error(npoly(x, degree=0), "degree")
  xl <- npoly(x, degree=3, raw=TRUE)
  expect_is(xl, "list")
  expect_false(is.data.frame(xl))

  k=3
  x <- npoly(x, degree=k) # raw FALSE by default
  expect_is(x, "coe_single")
  expect_is(x, "data.frame")
  expect_true(ncol(x)==k+1)
  expect_true(nrow(x)==1)

  # coo_list
  expect_error(npoly(bot$coo[1:5], degree=1), "bookstein")

  # mom
  expect_error(npoly(bot[1:2, ], degree=1), "bookstein")

  x <- olea[1:2, ]
  expect_message(npoly(x, degree=1, raw=TRUE), "useless")
})


test_that("opoly works", {
  expect_message(opoly("a"), "no method")
  x <- olea %>% pick(1)

  # degree default
  expect_message(opoly(x), "not provided")
  expect_message(opoly(olea$coo[1:2]), "not provided")
  expect_message(opoly(olea[1:2, ]), "not provided")

  expect_error(opoly(x, degree=0), "degree")
  xl <- opoly(x, degree=3, raw=TRUE)
  expect_is(xl, "list")
  expect_false(is.data.frame(xl))

  k=3
  x <- opoly(x, degree=k) # raw FALSE by default
  expect_is(x, "coe_single")
  expect_is(x, "data.frame")
  expect_true(ncol(x)==k+1)
  expect_true(nrow(x)==1)

  # coo_list
  expect_error(opoly(bot$coo[1:5], degree=1), "bookstein")

  # mom
  expect_error(opoly(bot[1:2, ], degree=1), "bookstein")

  x <- olea[1:2, ]
  expect_message(opoly(x, degree=1, raw=TRUE), "useless")
})




