context("Check code with calls")
library(tidyverse)
a <- function(x) x
b <- function(x) x

test_that("detect_mistakes detects surplus code", {
  
  # function
  user <-     quote(a(b(1)))
  solution <- quote(b(1))
  expect_equal(
               detect_mistakes(user, solution)
               , 
               surplus_call(quote(a()), quote(b(1)))
               )
  
  user <-     quote(a(b(1)))
  solution <- quote(a(1))
  expect_equal(
               detect_mistakes(user, solution)
               , 
               wrong_call(quote(b()), quote(1), quote(a()))
               )
  
  # non-function
  user <-     quote(1(a(1)))
  solution <- quote(a(1))
  expect_equal(
               detect_mistakes(user, solution)
               , 
               surplus_call("1()", quote(a(1)))
               )
  
  # internal atomic
  # arguments
  user <-     quote(b(1))
  solution <- quote(b())
  expect_equal(
               detect_mistakes(user, solution)
               , 
               surplus_argument(quote(b()), quote(1))
               )
  
  user <-     quote(b(x = 1))
  solution <- quote(b())
  expect_equal(
    detect_mistakes(user, solution)
    , 
    surplus_argument(quote(b()), quote(1), quote(x))
  )
  
  user <-     quote(b(x = a(1)))
  solution <- quote(b())
  expect_equal(
    detect_mistakes(user, solution)
    , 
    wrong_value(quote(1), quote(b())) # acceptable nudge to the right solution
  )
  
  # internal function
  user <-     quote(b(b(1)))
  solution <- quote(b(1))
  expect_equal(
    detect_mistakes(user, solution)
    , 
    surplus_call(quote(b()), quote(b(1)))
    )
  
  user <-     quote(a(b(1)))
  solution <- quote(a(1))
  expect_equal(
    detect_mistakes(user, solution)
    , 
    wrong_call(quote(b()), quote(1), quote(a()))
    )
  
  # internal non-function
  user <-     quote(a(1(1)))
  solution <- quote(a(1))
  expect_equal(
               detect_mistakes(user, solution)
               , 
               wrong_call("1()", quote(1), quote(a()))
               )
  
  # internal infix
  user <-     quote(b(1 + 2))
  solution <- quote(b(1))
  expect_equal(
               detect_mistakes(user, solution)
               , 
               wrong_call(quote(`+`), quote(1), quote(b()))
               )
})
