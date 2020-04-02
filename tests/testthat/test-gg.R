test_that("gg0 works", {
  expect_message(gg0("a"))
  # otherwise tested in examples
})

test_that("gg works", {
  expect_message(gg("a"))
  expect_is(bot2 %>% pick %>% gg(), "ggplot")
  # otherwise tested in examples
  # plot as gg # is that a good idea anyway?
  expect_is(bot2 %>% pick %>% plot(), "ggplot")
  expect_is(bot2 %>% pick %>% plot(col="red"), "ggplot")
})

test_that(".bind_distanciate_rows", {
  x <- bot2 %>% pick(1)
  y <- bot2 %>% pick(2)

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

  yy <- dplyr::slice(bot2, 10:12)$coo  # 3 shapes here
  zz <- .bind_distanciate_rows(unfold(x), unfold(yy))
  expect_is(zz, "tbl")
  expect_equal(unique(zz$group), 1:4)   # so expect 4 here
})

test_that("draw works", {
  bot2 %>% pick(1) %>% gg()

  # single shape
  x <- bot2 %>% pick(2) %>% draw()
  expect_is(x, "ggplot")

  # list
  bot2 %>% pick(1) %>% gg()
  x <- bot2$coo[1:10] %>% coo_list() %>% draw()
  expect_is(x, "ggplot")

  # list
  bot2 %>% pick(1) %>% gg()
  x <- bot2 %>% draw()
  expect_is(x, "ggplot")

})

test_that("mosaic works", {
  expect_is(bot2 %>% mosaic, "ggplot")
  expect_is(bot2 %>% mosaic(type), "ggplot")
  expect_is(bot2 %>% mosaic(fake, ncol=2), "ggplot")
  # otherwise tested in examples
})

test_that("pile works", {
  expect_is(bot2 %>% pile, "ggplot")
  expect_is(bot2 %>% pile(type), "ggplot")
  # otherwise tested in examples
})

