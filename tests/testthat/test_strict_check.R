library(tidyverse)
context("Spot differences")

test_that("Spots differences in atomics", {
  expect_match(strict_check("1", "1"), "Correct!")
  expect_equal(strict_check(quote(1), quote(2)), does_not_match(quote(1), quote(2)))
})

test_that("Spots differences in names", {
  y <- 5
  expect_equal(strict_check(quote(x), quote(x)), "Correct!")
  expect_equal(strict_check(quote(x), quote(y)), does_not_match(quote(x), quote(y)))
  expect_equal(strict_check(quote(5), quote(y)), does_not_match(quote(5), quote(y)))
})

test_that("Spots differences in calls", {
  a <- quote(map(lists, mean, na.rm = TRUE))
  b <- quote(map(vecs, mean, na.rm = TRUE))
  c <- quote(map(lists, mean))
  d <- quote(map(vecs, mean))

  expect_equal(strict_check(a, a), "Correct!")
  expect_equal(strict_check(a, b), does_not_match(quote(lists), quote(vecs), ".x"))
  expect_equal(strict_check(a, c), did_not_expect(a[[4]], .name = "na.rm"))
  expect_equal(strict_check(c, a), expected(a[[4]], .name = "na.rm"))
})

test_that("Mentions only first non-matching element", {
  w <- quote(1)
  x <- quote(log(1))
  y <- quote(sqrt(log(2)))
  z <- quote(sqrt(log(1)))

  expect_equal(strict_check(z, z), "Correct!")
  expect_equal(strict_check(w, z), does_not_match(w, z))
  expect_equal(strict_check(x, z), does_not_match(x, z))
  expect_equal(strict_check(y, z), does_not_match(quote(2), quote(1)))
})

test_that("Spots differences in argument names", {
  a <- quote(mean(1:10, trim = 1, na.rm = TRUE))
  b <- quote(mean(1:10, 1, TRUE))
  c <- quote(mean(1:10, cut = 1, na.rm = TRUE))

  expect_equal(strict_check(a, a), "Correct!")
  expect_equal(strict_check(b, a), expected(a[[3]], .name = "trim"))
  expect_equal(strict_check(c, a), did_not_expect(c[[3]], .name = "cut"))
})

test_that("Ignore differences in argument positions", {
  a <- quote(mean(1:10, trim = 1, na.rm = TRUE))
  b <- quote(mean(1:10, na.rm = TRUE, trim = 1))
  c <- quote(mean(1:10, na.rm = TRUE, 1))

  expect_equal(strict_check(a, a), "Correct!")
  expect_equal(strict_check(b, a), "Correct!")
  expect_equal(strict_check(c, a), expected(a[[3]], .name = "trim"))
})
