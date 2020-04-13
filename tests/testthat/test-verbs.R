test_that("pick works", {
  p1 <- bot %>% pick(1)
  p1_bis <- bot$coo[[1]]
  p1_ter <- dplyr::slice(bot, 1)$coo[[1]]

  expect_identical(p1, p1_bis)
  expect_identical(p1, p1_ter)

  expect_message(pick("a"))
  # id specified or not
  expect_is(bot$coo %>% pick(1), "coo_single")
  expect_is(bot$coo %>% pick(), "coo_single")
})

test_that("slice works", {
  b1 <- bot[1, ]
  b2 <- bot %>% slive(1)

  expect_identical(b1, b2)

  # no i passed, random
  expect_is(bot %>% slive(), "mom_tbl")
  # not defined
  expect_message(pick("a"))

  # classes
  expect_is(bot %>% slive(1), "mom_tbl")
  expect_is(iris %>% slive, "data.frame")
  expect_is(iris %>% tibble::as_tibble() %>% slive(), "tbl")

})

# test_that("plint works", {
#   x <- matrix(1:12, ncol=2) %>% coo_single()
#   # with no rhs
#   expect_output(plint(x))
#   expect_s3_class(plint(x), "coo_single")
# })

test_that("unfold works", {
  expect_message(unfold("a"))

  common <- function(x){
    expect_is(x, "tbl")
    expect_false(is(x, "mom_tbl"))
    expect_equal(coo_nb(x), 0)
    # also tests for 3 columns
    expect_true(all(colnames(x)[1:3] == c("x", "y", "group")))
  }

  # mom_tbl
  x <- bot %>% unfold()
  x %>% common()
  expect_equal(length(bot$coo), length(unique(x$group)))

  # and that with NULL names, unfold still works
  y <- bot
  names(y$coo) <- NULL
  x <- y %>% unfold
  x %>% common()
  expect_equal(length(y$coo), length(unique(x$group)))

  # single shape
  y <- pick(bot, 1)
  x <- unfold(y)
  x %>% common()
  expect_equal(nrow(y), nrow(x))
})

