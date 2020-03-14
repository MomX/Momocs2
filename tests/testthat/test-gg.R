test_that("gg0 works", {
  expect_message(gg0("a"))
  # otherwise tested in examples
})

test_that("gg works", {
  expect_message(gg("a"))
  expect_is(bot2 %>% pick %>% gg(), "ggplot")
  # otherwise tested in examples
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
  x <- bot2 %>% coo_center() %>% draw()
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

