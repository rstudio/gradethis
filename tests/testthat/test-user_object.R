user_code <- quote({
  x <- "I'm student code!"
  y <- list(1, 2, 3)
  z <- function() print("Hello World!")
})

solution_code <- quote({
  x <- "I'm solution code!"
  y <- list("a", "b", "c")
  z <- function() print("Goodnight Moon!")
})

exercise <- mock_this_exercise(!!user_code, !!solution_code)

test_that("can find user objects", {
  expect_equal(
    with_exercise(exercise, user_object_list()),
    c("x", "y", "z")
  )

  expect_true(with_exercise(exercise, user_object_exists("x")))
  expect_false(with_exercise(exercise, user_object_exists("invalid")))

  expect_equal(
    with_exercise(exercise, user_object_get("x")),
    "I'm student code!"
  )
  expect_equal(
    with_exercise(exercise, user_object_get("y")),
    list(1, 2, 3)
  )
  expect_equal(
    with_exercise(exercise, user_object_get("z")),
    function() print("Hello World!"),
    ignore_function_env = TRUE
  )
})

test_that("can find solution objects", {
  expect_equal(
    with_exercise(exercise, solution_object_list()),
    c("x", "y", "z")
  )

  expect_true(with_exercise(exercise, solution_object_exists("x")))
  expect_false(with_exercise(exercise, solution_object_exists("invalid")))

  expect_equal(
    with_exercise(exercise, solution_object_get("x")),
    "I'm solution code!"
  )
  expect_equal(
    with_exercise(exercise, solution_object_get("y")),
    list("a", "b", "c")
  )
  expect_equal(
    with_exercise(exercise, solution_object_get("z")),
    function() print("Goodnight Moon!"),
    ignore_function_env = TRUE
  )
})

test_that("mode", {
  expect_equal(
    with_exercise(exercise, user_object_list(mode = "character")),
    "x"
  )
  expect_equal(
    with_exercise(exercise, user_object_list(mode = "list")),
    "y"
  )
  expect_equal(
    with_exercise(exercise, user_object_list(mode = "function")),
    "z"
  )

  expect_true(
    with_exercise(exercise, user_object_exists("x", mode = "character"))
  )
  expect_false(
    with_exercise(exercise, user_object_exists("y", mode = "character"))
  )

  expect_equal(
    with_exercise(exercise, user_object_get("z", mode = "function")),
    function() print("Hello World!"),
    ignore_function_env = TRUE
  )

  suppressMessages(
    expect_feedback(
      with_exercise(exercise, user_object_get("y", mode = "function")),
      is_correct = logical(0),
      type = "warning"
    )
  )
})

test_that("setup code", {
  setup_code <- rlang::expr(setup_data <- mtcars)
  setup_exercise <- mock_this_exercise(
    !!user_code, !!solution_code, setup_exercise = !!setup_code
  )

  expect_equal(
    with_exercise(setup_exercise, user_object_list()),
    c("x", "y", "z")
  )
  expect_equal(
    with_exercise(setup_exercise, solution_object_list()),
    c("x", "y", "z")
  )

  expect_equal(
    with_exercise(setup_exercise, user_object_list(exclude_envir = NULL)),
    c("setup_data", "x", "y", "z")
  )

  expect_equal(
    with_exercise(setup_exercise, solution_object_list(exclude_envir = NULL)),
    c("setup_data", "x", "y", "z")
  )
})
