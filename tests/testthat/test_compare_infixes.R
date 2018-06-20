context("Check code with infix operators")
library(tidyverse)

test_that("compare_infixes detects missing code", {
  user <-     quote(1 + 2)
  solution <- quote(1 + 2 + 3)
  message <- expected_after("2", "+ 3")
  expect_equal(compare_infixes(user, solution), message)
})

test_that("compare_infixes detects surplus code", {
  user <-     quote(1 + 2 + 3)
  solution <- quote(1 + 2)
  message <- did_not_expect_after("2", "+ 3")
  expect_equal(compare_infixes(user, solution), message)
  
  user <-     quote(1 + 2)
  solution <- quote(1)
  message <- did_not_expect_after("1", "+ 2")
  expect_equal(compare_infixes(user, solution), message)
  
  user <-     quote(mean(1:10) + 2)
  solution <- quote(mean(1:10))
  message <- did_not_expect_after("mean(1:10)", "+ 2")
  expect_equal(compare_infixes(user, solution), message)  
  
  user <-     quote(1 - 2 + 2)
  solution <- quote(1 + 2)
  message <- does_not_match(as.symbol("-"), as.symbol("+"))
  expect_equal(compare_infixes(user, solution), message)  
  
  user <-     quote(1 - 2 + 3)
  solution <- quote(1 + 2 + 3)
  message <- does_not_match(as.symbol("-"), as.symbol("+"))
  expect_equal(compare_infixes(user, solution), message) 
  
  user <-     quote(1 + 2 + 3)
  solution <- quote(sum(1, 2) + 3)
  message <- does_not_match(as.symbol("1"), as.symbol("sum"))
  expect_equal(compare_infixes(user, solution), message) 
})

test_that("compare_infixes spots incorrect infix operators", {
  user <-     quote(1 - 2 + 3)
  solution <- quote(1 + 2)
  message <- does_not_match(as.symbol("-"), as.symbol("+"))
  expect_equal(compare_infixes(user, solution), message)
  
  user <-     quote(1 + 2 + 3)
  solution <- quote(1 + 2 - 3)
  message <- does_not_match(as.symbol("+"), as.symbol("-"))
  expect_equal(compare_infixes(user, solution), message)
  
  user <-     quote(1 + 2 + 3)
  solution <- quote(sum(1, 2, 3))
  message <- does_not_match(as.symbol("1"), as.symbol("sum"))
  expect_equal(compare_infixes(user, solution), message)
  
  user <-     quote(1 + 2 + 3)
  solution <- quote(sum(1 + 2, 3))
  message <- does_not_match(as.symbol("1"), as.symbol("sum"))
  expect_equal(compare_infixes(user, solution), message)
})

test_that("compare_infixes detects mismatched code on rhs", {
  user <-     quote(1 + mean(a))
  solution <- quote(1 + mean(b))
  message <- does_not_match(as.symbol("a"), as.symbol("b"))
  expect_equal(compare_infixes(user, solution), message)
  
  user <-     quote(1 + mean(a))
  solution <- quote(1 + median(a))
  message <- does_not_match(as.symbol("mean"), as.symbol("median"))
  expect_equal(compare_infixes(user, solution), message)
})

test_that("compare_infixes handles code with pipes", {
  user <-     quote(1:10 %>% mean() + 2)
  solution <- quote(mean(1:10) + 2)
  expect_null(compare_infixes(user, solution))
  
  user <-     quote(mean(1:10) + 2)
  solution <- quote(1:10 %>% mean() + 2)
  expect_null(compare_infixes(user, solution))
})
