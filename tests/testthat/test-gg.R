test_that("gg0 works", {
  expect_message(gg0("a"))
  # otherwise tested in examples
})

test_that("gg works", {
  expect_message(gg("a"))
  expect_is(bot %>% pick %>% gg(), "ggplot")
  # otherwise tested in examples
  # plot as gg # is that a good idea anyway?
  expect_is(bot %>% pick %>% plot(), "ggplot")
  expect_is(bot %>% pick %>% plot(col="red"), "ggplot")
})

test_that(".bind_distanciate_rows", {
  x <- bot %>% pick(1)
  y <- bot %>% pick(2)

  # x has no group column so far
  expect_error(.bind_distanciate_rows(x, y))
  x <- unfold(x)

  # now expected to work
  z <- .bind_distanciate_rows(x, y)
  expect_is(z, "tbl")
  expect_equal(unique(z$group), 1:2)

  # including when we unfold y before
  z <- .bind_distanciate_rows(x, unfold(y))
  expect_is(z, "tbl")
  expect_equal(unique(z$group), 1:2)

  yy <- dplyr::slice(bot, 10:12)$coo  # 3 shapes here
  zz <- .bind_distanciate_rows(unfold(x), unfold(yy))
  expect_is(zz, "tbl")
  expect_equal(unique(zz$group), 1:4)   # so expect 4 here
})

test_that("draw works", {
  bot %>% pick(1) %>% gg()

  # single shape
  x <- bot %>% pick(2) %>% draw()
  expect_is(x, "ggplot")

  # list
  bot %>% pick(1) %>% gg()
  x <- bot$coo[1:10] %>% draw()
  expect_is(x, "ggplot")

  # list
  bot %>% pick(1) %>% gg()
  x <- bot %>% draw()
  expect_is(x, "ggplot")

})

test_that("mosaic works", {
  expect_is(bot %>% mosaic, "ggplot")
  expect_is(bot %>% mosaic(fake, ncol=2), "ggplot")
  # otherwise tested in examples
})

test_that("pile works", {
  expect_is(bot %>% pile, "ggplot")
  expect_is(bot %>% pile(type), "ggplot")
  # otherwise tested in examples
})

test_that("inspect work, at least when it does not", {
  expect_message(inspect("a"), "no method")
})

