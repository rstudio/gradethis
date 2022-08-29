test_that("mock_this_exercise()", {
  ex1 <- mock_this_exercise(
    .user_code = x - y,
    .solution_code = {x + y}, # nolint: brace
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
  expect_graded(
    ex3$.solution,
    is_correct = logical(),
    msg = "No solution"
  )
})

test_that("mock_this_exercise() works with length-1 expressions", {
  ex <- mock_this_exercise(a, b)
  expect_equal(ex$.user_code, "a")
  expect_equal(ex$.solution_code, "b")

  ex2 <- mock_this_exercise({a}, {b}) # nolint: brace
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

test_that("mock_this_exercise() evaluates solution into check_env parent", {
  ex <- mock_this_exercise(
    "x <- 12",
    "y <- 30"
  )

  force(ex$.solution)

  expect_false(exists("x", ex))
  expect_true(exists("x", ex$.envir_result))

  expect_true(exists("y", ex))
  expect_false(exists("y", ex, inherits = FALSE))
  expect_true(exists("y", parent.env(ex), inherits = FALSE))
  expect_equal(get("y", ex), 30)
})

test_that("mock_this_exercise() mocks multiple solutions", {
  .solution_code_all <-
    gradethis_solutions(
      one = "runif(1)",
      two = "runif(2)",
      three = "runif(3)"
    )

  ex <- mock_this_exercise("runif(4)", !!format(.solution_code_all))
  # solution code round trips through exercise prep
  expect_equal(ex$.solution_code_all, .solution_code_all)

  expect_named(ex$.solution_code_all, c("one", "two", "three"))

  # default solution code is last solution
  expect_equal(ex$.solution_code, ex$.solution_code_all[[3]])
  expect_equal(length(ex$.solution), 3L)
})

test_that("mock_this_exercise() with non-R exercise engine", {
  ex <- mock_this_exercise("SELECT * FROM mtcars", .engine = "sql", .result = mtcars)

  expect_equal(ex$.engine, "sql")
  expect_equal(
    ex$.user_code,
    "SELECT * FROM mtcars"
  )
  expect_equal(
    ex$.result,
    mtcars
  )

  # .result is required if not R engine
  expect_error(
    mock_this_exercise("SELECT * FROM table", .engine = "sql")
  )

  # Bare code allowed only when R engine
  expect_error(
    mock_this_exercise(SELECT, .engine = "sql", .result = 7)
  )
})

test_that("mock_this_exercise(), non-R engine, no solution_eval_fn()", {
  ex <- mock_this_exercise(
    "apple",
    .engine = "echo_fruit",
    .result = "apple",
    .solution_code = "banana"
  )

  expect_graded(ex$.solution, logical(), "Solution results are not available")
  expect_graded(ex$.solution_all$solution, logical(), "Solution results are not available")
})

test_that("mock_this_exercise(), non-R engine, with solution_eval_fn()", {
  ex <-
    withr::with_options(
      list(gradethis.exercise_checker.solution_eval_fn = list(
        echo_fruit = function(code, envir) {
          code
        }
      )),
      mock_this_exercise(
        "apple",
        .engine = "echo_fruit",
        .result = "apple",
        .solution_code = "banana"
      )
    )

  expect_equal(ex$.solution, "banana")
  expect_equal(ex$.solution_all$solution, "banana")
})

test_that("mock_this_exercise(), non-R engine, with solution_eval_fn(), custom missing solution", {
  ex <-
    withr::with_options(
      list(gradethis.exercise_checker.solution_eval_fn = list(
        echo_fruit = function(code, envir) {
          if (nzchar(code)) return(code)
          rlang::abort(class = "error_missing_solution")
        }
      )),
      mock_this_exercise(
        "apple",
        .engine = "echo_fruit",
        .result = "apple",
        .solution_code = ""
      )
    )

  expect_graded(ex$.solution, logical(), "No solution is provided")
  expect_null(ex$.solution_all$solution)
})
