test_that("attributes works", {
  expect_null(iris %>% .add_history() %>% attr("history"))
  expect_equal(iris %>% .add_history("plop") %>% attr("history"), "plop")
  expect_identical(iris %>% .add_history("plop") %>% .add_history("plip") %>% attr("history"), c("plop", "plip"))
})
