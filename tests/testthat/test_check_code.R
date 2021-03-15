context("Check Code")

# these tests are largely redundant exercises that have been tested against detect_mistakes()

test_that("Spots differences in atomics", {

  expect_this_code("1", "1", is_correct = TRUE)

  expect_this_code("1", "2", is_correct = FALSE, msg = wrong_value(this = quote(1), that = quote(2)))
})

test_that("Spots differences in names", {
  x <- 1
  y <- 2
  expect_this_code("x", "x", is_correct = TRUE)
  expect_this_code("x", "y", is_correct = FALSE, msg = wrong_value(this = quote(x), that = quote(y)))
  expect_this_code("5", "y", is_correct = FALSE, msg = wrong_value(this = quote(5), that = quote(y)))
})

test_that("Spots differences in calls", {
  a <- "vapply(lists, mean, numeric(1), na.rm = TRUE)"
  b <- "vapply(vecs, mean, numeric(1), na.rm = TRUE)"
  c <- "vapply(lists, mean, numeric(1))"
  d <- "vapply(vecs, mean, numeric(1))"

  expect_this_code(a, a, is_correct = TRUE)
  expect_this_code(a, b, is_correct = FALSE, msg = wrong_value(this = quote(lists), that = quote(vecs)))

  expect_this_code(
    a, c,
    is_correct = FALSE,
    msg = surplus_argument(
      this_call = quote(vapply()),
      this_name = "na.rm",
      this = quote(TRUE)
    )
  )

  expect_this_code(
    c, a,
    is_correct = FALSE,
    msg = missing_argument(
      this_call = quote(vapply()),
      that_name = "na.rm"
    )
  )
})

test_that("Mentions only first non-matching element", {
  w <- "1"
  x <- "log(1)"
  y <- "sqrt(log(2))"
  z <- "sqrt(log(1))"

  expect_this_code(w, w, is_correct = TRUE)
  expect_this_code(w, z, is_correct = FALSE, msg = wrong_value(this = quote(1), that = quote(sqrt())))
  expect_this_code(x, z, is_correct = FALSE, msg = wrong_call(this = quote(log()), that = quote(sqrt())))
  expect_this_code(y, z, is_correct = FALSE, msg = wrong_value(this = quote(2), that = quote(1)))

})

test_that("Spots differences in argument names", {
  test_fn <- function(x, y = 1, z = 2, ...) {return(1)}

  a <- "test_fn(10, y = 1, z = TRUE)"
  b <- "test_fn(10, 1, TRUE)"
  c <- "test_fn(10, w = 1, z = TRUE)"

  expect_this_code(a, a, is_correct = TRUE)
  expect_this_code(b, a, is_correct = TRUE)
  expect_this_code(
    c, a,
    is_correct = FALSE,
    msg = surplus_argument(
      this_call = quote(test_fn()),
      this = 1,
      this_name = "w"
    )
  )

})

test_that("Ignore differences in argument positions (for non ... arguments)", {
  test_fn2 <- function(x, digits = 0){return(1)}
  a <- "test_fn2(x = pi, digits = 2)"
  b <- "test_fn2(pi, digits = 2)"
  c <- "test_fn2(2, x = pi)"
  d <- "test_fn2(digits = 2, x = pi)"

  expect_this_code(b, a, is_correct = TRUE)
  expect_this_code(c, a, is_correct = TRUE)
  expect_this_code(d, a, is_correct = TRUE)
  expect_this_code(a, d, is_correct = TRUE)

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
  # https://github.com/rstudio-education/grader/issues/28
  
  # stub tidyr::gather()
  gather <- function(key = "key", value = "value", ..., na.rm = FALSE) {
    NULL
  }

  user <- "gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = TRUE)" # nolint
  solution <- "gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = FALSE)" # nolint

  expect_this_code(
    user, solution,
    is_correct = FALSE,
    msg = wrong_value(quote(TRUE), that = quote(FALSE), this_name = quote(na.rm))
  )

  user <- "gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = TRUE)" # nolint
  solution <- "gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = TRUE)" # nolint
  expect_this_code(user, solution, is_correct = TRUE)

})
