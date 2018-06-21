context("Check code with calls")
library(tidyverse)
a <- function(x) x
b <- function(x) x

test_that("compare_calls detects surplus code", {
  
  # function
  user <-     quote(a(b(1)))
  solution <- quote(b(1))
  message <- does_not_match(quote(a), quote(b))
  expect_equal(compare_calls(user, solution), message)
  
  user <-     quote(a(b(1)))
  solution <- quote(a(1))
  message <- does_not_match(quote(b), quote(1))
  expect_equal(compare_calls(user, solution), message)
  
  # non-function
  user <-     quote(1(a(1)))
  solution <- quote(a(1))
  message <- not_a_call(quote(1))
  expect_equal(compare_calls(user, solution), message)
  
  # internal atomic
  
  # internal function
  user <-     quote(b(b(1)))
  solution <- quote(b(1))
  message <- does_not_match(quote(b), quote(1))
  expect_equal(compare_calls(user, solution), message)
  
  user <-     quote(a(b(1)))
  solution <- quote(a(1))
  message <- does_not_match(quote(b), quote(1))
  expect_equal(compare_calls(user, solution), message)
  
  # internal non-function
  user <-     quote(a(1(1)))
  solution <- quote(a(1))
  message <- not_a_call(quote(1))
  expect_equal(compare_calls(user, solution), message)
  
  # internal infix
  user <-     quote(b(1 + 2))
  solution <- quote(b(1))
  message <- did_not_expect_after(quote(1), quote( "+ 2"))
  expect_equal(compare_calls(user, solution), message)
  
  # internal pipe
  user <-     quote(b(1 %>% abs()))
  solution <- quote(b(1))
  message <- did_not_expect_after(quote(1), quote( "%>% abs()"))
  expect_equal(compare_calls(user, solution), message)
})

test_that("compare_calls detects missing code", {
  
  # function
  user <-     quote(b(1))
  solution <- quote(a(b(1)))
  message <- does_not_match(quote(b), quote(a))
  expect_equal(compare_calls(user, solution), message)
  
  # non-function
  user <-     quote(1(1))
  solution <- quote(a(b(1)))
  message <- not_a_call(quote(1))
  expect_equal(compare_calls(user, solution), message)
  
  # internal atomic
  user <-     quote(a())
  solution <- quote(a(1))
  message <- expected(quote(1))
  expect_equal(compare_calls(user, solution), message)
  
  # internal function
  user <-     quote(a(1))
  solution <- quote(a(b(1)))
  message <- expected_you_to_call(quote(1), "b()")
  expect_equal(compare_calls(user, solution), message)
  
  # internal non-function would not appear in a solution
  
  # internal infix
  user <-     quote(sqrt(1))
  solution <- quote(sqrt(1 + 2))
  message <- expected_after(quote(1), quote("+ 2"))
  expect_equal(compare_calls(user, solution), message)
  
  user <-     quote(sqrt(1))
  solution <- quote(sqrt(1 + 2 + 3))
  message <- expected_after(quote(1), quote("+ 2"))
  expect_equal(compare_calls(user, solution), message)
  
  user <-     quote(sqrt(1 + 2))
  solution <- quote(sqrt(1 + 2 + 3))
  message <- expected_after(quote(2), quote("+ 3"))
  expect_equal(compare_calls(user, solution), message)
  
  # internal pipe
  user <-     quote(sqrt(1))
  solution <- quote(sqrt(1 %>% log()))
  message <- expected_after(quote(1), quote("%>% log()"))
  expect_equal(compare_calls(user, solution), message)
  
  user <-     quote(sqrt(1))
  solution <- quote(sqrt(1 %>% log() %>% abs()))
  message <- expected_after(quote(1), quote("%>% log()"))
  expect_equal(compare_calls(user, solution), message)
  
  user <-     quote(sqrt(1 %>% log()))
  solution <- quote(sqrt(1 %>% log() %>% abs()))
  message <- expected_after(quote(log()), quote("%>% abs()"))
  expect_equal(compare_calls(user, solution), message)
  
  user <-     quote(sqrt(1 + 2))
  solution <- quote(sqrt(1 + 2 %>% log()))
  message <- expected_after(quote(2), quote("%>% log()"))
  expect_equal(compare_calls(user, solution), message)
})

test_that("compare_calls detects mis-matched code", {
  
  # function
  user <-     quote(b(1))
  solution <- quote(a(1))
  message <- does_not_match(quote(b), quote(a))
  expect_equal(compare_calls(user, solution), message)
  
  # non-function
  user <-     quote(1(1))
  solution <- quote(a(1))
  message <- not_a_call(quote(1))
  expect_equal(compare_calls(user, solution), message)
  
  # internal atomic
  user <-     quote(a(1))
  solution <- quote(a(2))
  message <- does_not_match(quote(1), quote(2))
  expect_equal(compare_calls(user, solution), message)
  
  # internal function
  user <-     quote(a(b(1)))
  solution <- quote(a(c(1)))
  message <- does_not_match(quote(b), quote(c))
  expect_equal(compare_calls(user, solution), message)
  
  # internal non-function
  user <-     quote(a(1(1)))
  solution <- quote(a(b(1)))
  message <- not_a_call(quote(1))
  expect_equal(compare_calls(user, solution), message)
  
  # internal infix
  user <-     quote(a(1 + 2))
  solution <- quote(a(1 + 3))
  message <- does_not_match(quote(2), quote(3))
  expect_equal(compare_calls(user, solution), message)
  
  user <-     quote(a(1 + 2 + 4))
  solution <- quote(a(1 + 3 + 4))
  message <- does_not_match(quote(2), quote(3))
  expect_equal(compare_calls(user, solution), message)
  
  user <-     quote(a(1 + 2 + 4))
  solution <- quote(a(1 + 3 + 5))
  message <- does_not_match(quote(2), quote(3))
  expect_equal(compare_calls(user, solution), message)
  
  user <-     quote(a(2 + 1))
  solution <- quote(a(3 + 1))
  message <- does_not_match(quote(2), quote(3))
  expect_equal(compare_calls(user, solution), message)
  
  user <-     quote(a(1 + 1))
  solution <- quote(a(1 - 1))
  message <- does_not_match(quote("+"), quote("-"))
  expect_equal(compare_calls(user, solution), message)
  
  user <-     quote(a(1 + 1 + 1))
  solution <- quote(a(1 - 1 + 1))
  message <- does_not_match(quote("+"), quote("-"))
  expect_equal(compare_calls(user, solution), message)
  
  # internal pipe
  user <-     quote(a(2 %>% abs()))
  solution <- quote(a(2 %>% log()))
  message <- does_not_match(quote(abs()), quote(log()))
  expect_equal(compare_calls(user, solution), message)
  
  user <-     quote(a(2 %>% abs() %>% sqrt()))
  solution <- quote(a(2 %>% log() %>% sqrt()))
  message <- does_not_match(quote(abs()), quote(log()))
  expect_equal(compare_calls(user, solution), message)
  
  user <-     quote(a(2 %>% abs()))
  solution <- quote(a(2 + log(1)))
  message <- expected_infix_after("2", "+", "abs")
  expect_equal(compare_calls(user, solution), message)
})