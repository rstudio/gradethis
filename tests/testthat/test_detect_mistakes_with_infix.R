library(tidyverse)
context("Check code with infix operators")

test_that("strict_check() returns sensible message for code with infix operator", {
  
  expect_equal(expected_after("2", "+ 3"),
               detect_mistakes_with_infix(user = quote(1 + 2), 
                                          solution = quote(1 + 2 + 3))) 
  
  expect_equal(did_not_expect_after("2", "+ 3"),
               detect_mistakes_with_infix(user = quote(1 + 2 + 3), 
                                          solution = quote(1 + 2)))
  
  expect_equal(did_not_expect_after("1", "- 2"),
               detect_mistakes_with_infix(user = quote(1 - 2 + 3), 
                                          solution = quote(1 + 2)))
})