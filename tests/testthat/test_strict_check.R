library(tidyverse)
context("Spot differences")

test_that("Spots differences in atomics", {
  expect_match(strict_check(user = "1", solution = "1"), "Correct!")
  expect_equal(strict_check(user = quote(1), solution = quote(2)), does_not_match(quote(1), quote(2)))
})

test_that("Spots differences in names", {
  y <- 5
  expect_equal(strict_check(user = quote(x), solution = quote(x)), "Correct!")
  expect_equal(strict_check(user = quote(x), solution = quote(y)), does_not_match(quote(x), quote(y)))
  expect_equal(strict_check(user = quote(5), solution = quote(y)), does_not_match(quote(5), quote(y)))
})

test_that("Spots differences in calls", {
  a <- quote(map(lists, mean, na.rm = TRUE))
  b <- quote(map(vecs, mean, na.rm = TRUE))
  c <- quote(map(lists, mean))
  d <- quote(map(vecs, mean))

  expect_equal(strict_check(user = a, solution = a), "Correct!")
  expect_equal(strict_check(user = a, solution = b), does_not_match(quote(lists), quote(vecs)))
  expect_equal(strict_check(user = a, solution = c), did_not_expect(a[[4]], .name = "na.rm"))
  expect_equal(strict_check(user = c, solution = a), expected(a[[4]], .name = "na.rm"))
})

test_that("Mentions only first non-matching element", {
  w <- quote(1)
  x <- quote(log(1))
  y <- quote(sqrt(log(2)))
  z <- quote(sqrt(log(1)))

  expect_equal(strict_check(user = z, solution = z), "Correct!")
  expect_equal(strict_check(user = w, solution = z), does_not_match(w, z))
  expect_equal(strict_check(user = x, solution = z), does_not_match(x, z))
  expect_equal(strict_check(user = y, solution = z), does_not_match(quote(2), quote(1)))
})

test_that("Spots differences in argument names", {
  a <- quote(mean(1:10, trim = 1, na.rm = TRUE))
  b <- quote(mean(1:10, 1, TRUE))
  c <- quote(mean(1:10, cut = 1, na.rm = TRUE))

  expect_equal(strict_check(user = a, solution = a), "Correct!")
  expect_equal(strict_check(user = b, solution = a), expected(a[[3]], .name = "trim"))
  expect_equal(strict_check(user = c, solution = a), did_not_expect(c[[3]], .name = "cut"))
})

test_that("Ignore differences in argument positions", {
  a <- quote(mean(1:10, trim = 1, na.rm = TRUE))
  b <- quote(mean(1:10, na.rm = TRUE, trim = 1))
  c <- quote(mean(1:10, na.rm = TRUE, 1))
  d <- quote(mean(1:10))

  expect_equal(strict_check(user = a, solution = a), "Correct!")
  expect_equal(strict_check(user = b, solution = a), "Correct!")
  expect_equal(strict_check(user = c, solution = a), expected(a[[3]], .name = "trim"))
  expect_equal(strict_check(user = d, solution = a), expected(a[[3]], .name = "trim"))
  expect_equal(strict_check(user = a, solution = d), did_not_expect(a[[3]], .name = "trim"))
})

test_that("Returns intelligent error when no solution code", {
  expect_error(strict_check(), "No solution is provided for this exercise.")
})

test_that("Returns intelligent error when no user code", {
  expect_error(strict_check(solution = quote(5)), "I didn't receive your code. Did you write any?")
})

test_that("Spot differences when pipes are involved", {
  pipe <- quote(iris %>% filter(Species == "Virginica") %>% select(Sepal.Length))
  func <- quote(select(iris %>% filter(Species == "Virginica"), Sepal.Length))
  func1 <- quote(select(filter(iris, Species == "Virginica"), Sepal.Length))
  pipe1 <- quote(iris %>% filter(Species == "Virginica") %>% select(Petal.Length))
  pipe2 <- quote(iris %>% arrange(Species) %>% select(Sepal.Length))
  pipe3 <- quote(iris %>% lm(Sepal.Length ~ Sepal.Width, data = .))
  func3 <- quote(lm(Sepal.Length ~ Sepal.Width, data = iris))
  
  expect_equal(strict_check(user = func,  solution = pipe), "Correct!")
  expect_equal(strict_check(user = func1, solution = pipe), "Correct!")
  expect_equal(strict_check(user = pipe,  solution = func), "Correct!")
  expect_equal(strict_check(user = pipe,  solution = func1), "Correct!")
  expect_equal(strict_check(user = pipe,  solution = pipe), "Correct!")
  expect_equal(strict_check(user = func,  solution = func1), "Correct!")
  expect_equal(strict_check(user = func1, solution = func1), "Correct!")
  
  expect_equal(strict_check(user = pipe1, solution = pipe), does_not_match(quote(Petal.Length), quote(Sepal.Length)))
  expect_equal(strict_check(user = pipe1, solution = func), does_not_match(quote(Petal.Length), quote(Sepal.Length)))
  expect_equal(strict_check(user = pipe1, solution = func1), does_not_match(quote(Petal.Length), quote(Sepal.Length)))
  expect_equal(strict_check(user = pipe2, solution = pipe), does_not_match(quote(arrange), quote(filter)))
  expect_equal(strict_check(user = pipe2, solution = func), does_not_match(quote(arrange), quote(filter)))
  expect_equal(strict_check(user = pipe2, solution = func1), does_not_match(quote(arrange), quote(filter)))
  
  
  expect_equal(strict_check(user = func3, solution = pipe3), "Correct!")
  expect_equal(strict_check(user = pipe3, solution = func3), "Correct!")
  expect_equal(strict_check(user = pipe3, solution = pipe3), "Correct!")
  
})


