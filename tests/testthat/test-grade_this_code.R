# these tests are largely redundant exercises that have been tested against detect_mistakes()

test_that("Spots differences in atomics", {

  expect_this_code("1", "1", is_correct = TRUE)

  expect_this_code(
    "1", "2", is_correct = FALSE,
    msg = wrong_value(submitted = quote(1), solution = quote(2))
  )
})

test_that("Spots differences in names", {
  x <- 1
  y <- 2
  expect_this_code("x", "x", is_correct = TRUE)
  expect_this_code(
    "x", "y", is_correct = FALSE,
    msg = wrong_value(submitted = quote(x), solution = quote(y))
  )
  expect_this_code(
    "5", "y", is_correct = FALSE,
    msg = wrong_value(submitted = quote(5), solution = quote(y))
  )
})

test_that("Spots differences in calls", {
  a <- "vapply(lists, mean, numeric(1), na.rm = TRUE)"
  b <- "vapply(vecs, mean, numeric(1), na.rm = TRUE)"
  c <- "vapply(lists, mean, numeric(1))"
  d <- "vapply(vecs, mean, numeric(1))"

  expect_this_code(a, a, is_correct = TRUE)
  expect_this_code(
    a, b, is_correct = FALSE,
    msg = wrong_value(submitted = quote(lists), solution = quote(vecs))
  )

  expect_this_code(
    a, c,
    is_correct = FALSE,
    msg = surplus_argument(
      submitted_call = quote(vapply()),
      submitted_name = "na.rm",
      submitted = quote(TRUE)
    )
  )

  expect_this_code(
    c, a,
    is_correct = FALSE,
    msg = missing_argument(
      submitted_call = quote(vapply()),
      solution_name = "na.rm"
    )
  )
})

test_that("Mentions only first non-matching element", {
  w <- "1"
  x <- "log(1)"
  y <- "sqrt(log(2))"
  z <- "sqrt(log(1))"

  expect_this_code(w, w, is_correct = TRUE)
  expect_this_code(
    w, z, is_correct = FALSE,
    msg = wrong_value(submitted = quote(1), solution = quote(sqrt()))
  )
  expect_this_code(
    x, z, is_correct = FALSE,
    msg = wrong_call(submitted = quote(log()), solution = quote(sqrt()))
  )
  expect_this_code(
    y, z, is_correct = FALSE,
    msg = wrong_value(submitted = quote(2), solution = quote(1))
  )

})

test_that("Spots differences in argument names", {
  setup <- "test_fn <- function(x, y = 1, z = 2, ...) {return(1)}"

  a <- "test_fn(10, y = 1, z = TRUE)"
  b <- "test_fn(10, 1, TRUE)"
  c <- "test_fn(10, w = 1, z = TRUE)"

  expect_this_code(a, a, is_correct = TRUE, setup_exercise = !!setup)
  expect_this_code(b, a, is_correct = TRUE, setup_exercise = !!setup)
  expect_this_code(
    c, a,
    is_correct = FALSE,
    msg = surplus_argument(
      submitted_call = quote(test_fn()),
      submitted = 1,
      submitted_name = "w"
    ),
    setup_exercise = !!setup
  )

})

test_that("Ignore differences in argument positions (for non ... arguments)", {
  setup_fn <- "test_fn2 <- function(x, digits = 0){return(1)}"
  a <- "test_fn2(x = pi, digits = 2)"
  b <- "test_fn2(pi, digits = 2)"
  c <- "test_fn2(2, x = pi)"
  d <- "test_fn2(digits = 2, x = pi)"

  expect_this_code(b, a, is_correct = TRUE, setup_exercise = !!setup_fn)
  expect_this_code(c, a, is_correct = TRUE, setup_exercise = !!setup_fn)
  expect_this_code(d, a, is_correct = TRUE, setup_exercise = !!setup_fn)
  expect_this_code(a, d, is_correct = TRUE, setup_exercise = !!setup_fn)

})

test_that("Error is when trying to call code_feedback with no solution", {
  testthat::expect_error(
    code_feedback(user_code = "5", solution_code = NULL)
  )
})

test_that("Returns intelligent error when no user code", {
  testthat::expect_error(
    code_feedback(user_code = NULL, solution_code = "5")
  )
})

test_that("code_feedback() can handle character vectors", {
  expect_grade_code(
    user_code = c("1", "3"),
    solution_code = c("1", "2"),
    is_correct = FALSE,
    msg = "I expected `2` where you wrote `3`."
  )
  expect_null(code_feedback(c("1", "2"), c("1", "2")))
  expect_error(code_feedback("1", NULL))
  expect_error(code_feedback("1", character()))
})

test_that("Spot differences when pipes are involved", {

  select <- function(df, x) {
    df[[1]]
  }
  filter <- subset
  arrange <- function(df, ...) {
    df
  }

  func <- "log(1:10 %>% mean(na.rm = TRUE), base = 10)"
  pipe <- "1:10 %>% mean(na.rm = TRUE) %>% log(base = 10)"
  func1 <- "log(mean(1:10, na.rm = TRUE), base = 10)"
  pipe3 <- "iris %>% lm(Sepal.Length ~ Sepal.Width, data = .)"
  func3 <- "lm(Sepal.Length ~ Sepal.Width, data = iris)"

  expect_this_code(func,  pipe, is_correct = TRUE)
  expect_this_code(func1, pipe, is_correct = TRUE)
  expect_this_code(pipe,  func, is_correct = TRUE)
  expect_this_code(pipe,  func1, is_correct = TRUE)
  expect_this_code(pipe,  pipe, is_correct = TRUE)
  expect_this_code(func,  func1, is_correct = TRUE)
  expect_this_code(func1, func1, is_correct = TRUE)
  expect_this_code(func3, pipe3, is_correct = TRUE)
  expect_this_code(pipe3, func3, is_correct = TRUE)
  expect_this_code(pipe3, pipe3, is_correct = TRUE)

})

test_that("Spots differences in long calls", {
  # original discussion here:
  # https://github.com/rstudio/gradethis/issues/28

  # stub tidyr::gather()
  gather <- function(key = "key", value = "value", ..., na.rm = FALSE) { # nolint: object_name
    NULL
  }

  user <- "gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = TRUE)" # nolint
  solution <- "gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = FALSE)" # nolint

  expect_this_code(
    user, solution,
    is_correct = FALSE,
    msg = wrong_value(submitted = quote(TRUE), solution = quote(FALSE), submitted_name = quote(na.rm))
  )

  user <- "gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = TRUE)" # nolint
  solution <- "gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = TRUE)" # nolint
  expect_this_code(user, solution, is_correct = TRUE)
})

test_that("grade_this_code() gives neutral feedback if code is missing", {
  expect_graded(
    grade_this_code()(
      mock_this_exercise(.user_code = "", .solution_code = "rnorm(1)")
    ),
    is_correct = logical()
  )
})

test_that("grade_this_code() doesn't have to return a grade", {
  # user code is incorrect
  ex_fails <- mock_this_exercise("1", "2")
  expect_graded(grade_this_code(action = "both")(ex_fails), is_correct = FALSE)
  expect_graded(grade_this_code(action = "fail")(ex_fails), is_correct = FALSE)
  expect_null(grade_this_code(action = "pass")(ex_fails))

  # user code is correct
  ex_ok <- mock_this_exercise("1 + 1", "1 + 1")
  expect_graded(grade_this_code(action = "both")(ex_ok), is_correct = TRUE)
  expect_graded(grade_this_code(action = "pass")(ex_ok), is_correct = TRUE)
  expect_null(grade_this_code(action = "fail")(ex_ok))
})

test_that("grade_this_code() doesn't duplicate feedback", {
  ex <- mock_this_exercise("1", "2")
  feedback <- code_feedback(ex$.user_code, ex$.solution_code)

  withr::with_options(
    list(gradethis.fail.hint = FALSE),
    expect_equal(
      str_count(grade_this_code()(ex)$message, fixed(feedback)),
      1
    )
  )

  withr::with_options(
    list(gradethis.fail.hint = TRUE),
    expect_equal(
      str_count(grade_this_code()(ex)$message, fixed(feedback)),
      1
    )
  )
})

test_that("maybe_code_feedback() always gives feedback in grade_this_code()", {
  ex <- mock_this_exercise("1", "2")
  feedback <- code_feedback(ex$.user_code, ex$.solution_code)

  withr::with_options(
    list(gradethis.maybe_code_feedback = TRUE),
    expect_equal(
      str_count(
        grade_this_code(incorrect = "{maybe_code_feedback()}")(ex)$message,
        fixed(feedback)
      ),
      1
    )
  )

  withr::with_options(
    list(gradethis.maybe_code_feedback = FALSE),
    expect_equal(
      str_count(
        grade_this_code(incorrect = "{maybe_code_feedback()}")(ex)$message,
        fixed(feedback)
      ),
      1
    )
  )

  expect_equal(
    grade_this_code(incorrect = "incorrect")(ex)$message,
    "incorrect"
  )
})
