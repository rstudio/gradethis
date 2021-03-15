test_that("mock_this_exercise()", {
  ex1 <- mock_this_exercise(
    .user_code = x - y,
    .solution_code = {x + y},
    setup_global = x <- 31,
    setup_exercise = {
      z <- 0
      y <- 11
    }
  )
  ex2 <- mock_this_exercise(
    .user_code = "z <- 0\nx - y",
    .solution_code = "x + y",
    setup_global = "x <- 31",
    setup_exercise = "y <- 11"
  )
  ex3 <- mock_this_exercise(
    .user_code = {
      z <- 0
      x - y
    },
    setup_global = x <- 31,
    setup_exercise = y <- 11
  )
  
  expect_equal(ex1$.result, ex2$.user)
  expect_equal(ex2$.user, ex3$.last_value)
  expect_equal(ex1$.user_code, "x - y")
  expect_equal(ex2$.user_code, "z <- 0\nx - y")
  expect_equal(ex2$.user_code, ex3$.user_code)
  
  # Prep environment
  # has access to `x` from parent env
  expect_equal(get("x", ex1$.envir_prep), 31)
  expect_equal(get("x", ex2$.envir_prep), 31)
  expect_equal(get("x", ex3$.envir_prep), 31)
  
  # has variables from setup_exercise
  expect_equal(ls(ex1$.envir_prep), c("y", "z"))
  expect_equal(ls(ex2$.envir_prep), c("y"))
  expect_equal(ls(ex3$.envir_prep), c("y"))
  
  # Result environment
  # has access to `x`, `y` and `z` through env chain
  expect_equal(get("x", ex1$.envir_prep), 31)
  expect_equal(get("x", ex2$.envir_prep), 31)
  expect_equal(get("x", ex3$.envir_prep), 31)
  
  expect_equal(get("y", ex1$.envir_result), 11)
  expect_equal(get("y", ex2$.envir_result), 11)
  expect_equal(get("y", ex3$.envir_result), 11)
  
  expect_equal(get("z", ex1$.envir_result), 0)
  expect_equal(get("z", ex2$.envir_result), 0)
  expect_equal(get("z", ex3$.envir_result), 0)
  
  # result env is prep + result
  expect_equal(ls(ex1$.envir_result), c("y", "z"))
  expect_equal(ls(ex2$.envir_result), c("y", "z"))
  expect_equal(ls(ex3$.envir_result), c("y", "z"))
  
  # Solution Code
  expect_equal(ex1$.solution_code, "x + y")
  expect_equal(ex1$.solution_code, ex2$.solution_code)
  
  expect_equal(ex1$.solution, 42)
  expect_equal(ex2$.solution, 42)
  
  # no solution, delayed assignment of solution throws fail()
  expect_equal(
    ex3$.solution,
    fail(I("No solution is provided for this exercise."))
  )
})

test_that("mock_this_exercise() works with length-1 expressions", {
  ex <- mock_this_exercise(a, b)
  expect_equal(ex$.user_code, "a")
  expect_equal(ex$.solution_code, "b")
  
  ex2 <- mock_this_exercise({a}, {b})
  expect_equal(ex$.user_code, "a")
  expect_equal(ex$.solution_code, "b")
})

test_that("user error populates .error, .result, .last_value, .user", {
  expect_condition_message <- function(cond, message, ...) {
    expect_s3_class(cond, "condition")
    expect_match(cond$message, message, ...)
  }
  ex <- mock_this_exercise(stop("boom"))
  expect_condition_message(ex$.error, "boom")
  expect_condition_message(ex$.result, "boom")
  expect_condition_message(ex$.last_value, "boom")
  expect_condition_message(ex$.user, "boom")
})
