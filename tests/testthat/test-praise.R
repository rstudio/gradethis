test_that("random_praise() returns a string", {
  expect_equal(length(random_praise()), 1L)
  expect_type(random_praise(), "character")
})

test_that("random_encouragement() returns a string", {
  expect_equal(length(random_encouragement()), 1L)
  expect_type(random_encouragement(), "character")
})

test_that("random_encourag() is deprecated", {
  expect_warning(random_encourage())
})
