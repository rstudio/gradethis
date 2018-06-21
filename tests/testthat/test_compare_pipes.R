context("Check code with pipes")
library(tidyverse)

test_that("compare_pipes identifies an extra pipe", {
  user <-     quote(iris %>% pluck("Sepal.Length") %>% mean()) 
  solution <- quote(iris %>% pluck("Sepal.Length"))
  excess <- paste(deparse(user[[1]]), deparse(user[[3]]))
  message <- grader:::did_not_expect_after(user[[2]], excess)
  
  expect_equal(compare_pipes(user, solution), message)
  
  solution <- quote(pluck(iris, "Sepal.Length")) 
  user <-     quote(iris %>% pluck("Sepal.Length") %>% mean())
  excess <- paste(deparse(user[[1]]), deparse(user[[3]]))
  message <- grader:::did_not_expect_after(user[[2]], excess)
  
  expect_equal(compare_pipes(user, solution), message)

  solution <- quote(iris) 
  user <-     quote(iris %>% pluck("Sepal.Length"))
  excess <- paste(deparse(user[[1]]), deparse(user[[3]]))
  message <- grader:::did_not_expect_after(user[[2]], excess)
  
  expect_equal(compare_pipes(user, solution), message)
  
  user <-     quote(1 %>% log())
  solution <- quote(1 %>% log() %>% abs())
  message <- expected_after(quote(log()), quote("%>% abs()"))
  expect_equal(compare_calls(user, solution), message)
})

test_that("compare_pipes identifies a missing pipe", {
  user <-     quote(iris %>% pluck("Sepal.Length")) 
  solution <- quote(iris %>% pluck("Sepal.Length") %>% mean())
  
  missing <- paste(deparse(solution[[1]]), deparse(solution[[3]]))
  message <- grader:::expected_after(user[[3]], missing)
  
  expect_equal(compare_pipes(user, solution), message)
  
})

test_that("compare_pipes spots correct code", {
  user <-     quote(iris %>% pluck("Sepal.Length") %>% mean()) 
  solution <- quote(iris %>% pluck("Sepal.Length") %>% mean())
  
  expect_null(compare_pipes(user, solution))
  
  user <-     quote(iris %>% pluck("Sepal.Length") %>% mean()) 
  solution <- quote(mean(pluck(iris, "Sepal.Length")))
  
  expect_null(compare_pipes(user, solution))
  
  user <-     quote(iris %>% pluck("Sepal.Length") %>% mean(TRUE)) 
  solution <- quote(mean(pluck(iris, "Sepal.Length"), na.rm = TRUE))
  
  expect_null(compare_pipes(user, solution))
  
})

test_that("compare_pipes spots mismatched code", {
  user <-     quote(iris %>% pluck("Sepal.Length") %>% mean()) 
  solution <- quote(iris %>% pluck("Sepal.Length") %>% median())
  
  expect_equal(compare_pipes(user, solution),
               grader:::does_not_match(user[[3]], solution[[3]]))
  
  user <-     quote(iris %>% pluck("Sepal.Length") %>% mean()) 
  solution <- quote(iris %>% pluck("Sepal.Width") %>% median())
  
  expect_equal(compare_pipes(user, solution),
               grader:::does_not_match("Sepal.Length", "Sepal.Width"))
})
