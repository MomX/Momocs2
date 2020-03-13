test_that("gg0 works", {
  expect_message(gg0("a"))
  # otherwise tested in examples
})

test_that("gg works", {
  expect_message(gg("a"))
  expect_is(bot2 %>% pick %>% gg(), "ggplot")
  # otherwise tested in examples
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

