context("Legacy Check Code")

# these tests are largely redundant exercises that have been tested against detect_mistakes()

test_that("Spots differences in atomics", {

  expect_grade_code(
    user_code = "1",
    solution_code = "1",
    is_correct = TRUE
  )

  expect_grade_code(
    user_code = "1",
    solution_code = "2",
    is_correct = FALSE
  )
})

test_that("Spots differences in names", {

  expect_grade_code(
    user_code = "x",
    solution_code = "x",
    is_correct = TRUE
  )

  expect_grade_code(
    user_code = "x",
    solution_code = "y",
    is_correct = FALSE,
    msg = wrong_value(this = quote(x), that = quote(y))
  )

  expect_grade_code(
    user_code = "5",
    solution_code = "y",
    is_correct = FALSE,
    msg = wrong_value(this = quote(5), that = quote(y))
  )
})

test_that("Spots differences in calls", {
  a <- expression(vapply(lists, mean, numeric(1), na.rm = TRUE))
  b <- expression(vapply(vecs, mean, numeric(1), na.rm = TRUE))
  c <- expression(vapply(lists, mean, numeric(1)))
  d <- expression(vapply(vecs, mean, numeric(1)))

  expect_grade_code(
    user_code = a,
    solution_code = a,
    is_correct = TRUE
  )

  expect_grade_code(
    glue_correct = '{ .message } { .correct }',
    glue_incorrect = '{ .message } { .incorrect }',
    user_code = a,
    solution_code = b,
    is_correct = FALSE,
    msg = wrong_value(this = quote(lists), that = quote(vecs))
  )

  expect_grade_code(
    user_code = a,
    solution_code = c,
    is_correct = FALSE,
    msg = surplus_argument(
      this_call = quote(vapply()),
      this_name = "na.rm",
      this = quote(TRUE)
    )
  )

  expect_grade_code(
    user_code = c,
    solution_code = a,
    is_correct = FALSE,
    msg = missing_argument(
      this_call = quote(vapply()),
      that_name = "na.rm"
    )
  )

})

test_that("Mentions only first non-matching element", {
  w <- expression(1)
  x <- expression(log(1))
  y <- expression(sqrt(log(2)))
  z <- expression(sqrt(log(1)))

  expect_grade_code(
    user_code = w,
    solution_code = w,
    is_correct = TRUE
  )

  expect_grade_code(
    user_code = w,
    solution_code = z,
    is_correct = FALSE,
    msg = wrong_value(this = quote(1), that = quote(sqrt()))
  )

  expect_grade_code(
    user_code = x,
    solution_code = z,
    is_correct = FALSE,
    msg = wrong_call(this = quote(log()), that = quote(sqrt()))
  )

  expect_grade_code(
    user_code = y,
    solution_code = z,
    is_correct = FALSE,
    msg = wrong_value(this = quote(2), that = quote(1))
  )
})

test_that("Spots differences in argument names", {
  test_fn <- function(x, y = 1, z = 2, ...) {return(1)}

  a <- expression(test_fn(10, y = 1, z = TRUE))
  b <- expression(test_fn(10, 1, TRUE))
  c <- expression(test_fn(10, w = 1, z = TRUE))


  expect_grade_code(
    user_code = a,
    solution_code = a,
    is_correct = TRUE
  )

  expect_grade_code(
    user_code = b,
    solution_code = a,
    is_correct = TRUE
  )

  expect_grade_code(
    user_code = c,
    solution_code = a,
    is_correct = FALSE,
    msg = surplus_argument(
      this_call = c[[1]],
      this = 1,
      this_name = "w"
    )
  )
})

test_that("Ignore differences in argument positions (for non ... arguments)", {
  test_fn <- function(x, digits = 0){return(1)}
  a <- expression(test_fn(x = pi, digits = 2))
  b <- expression(test_fn(pi, digits = 2))
  c <- expression(test_fn(2, x = pi))
  d <- expression(test_fn(digits = 2, x = pi))

  expect_grade_code(
    user_code = a,
    solution_code = d,
    is_correct = TRUE
  )

  expect_grade_code(
    user_code = b,
    solution_code = a,
    is_correct = TRUE
  )

  expect_grade_code(
    user_code = c,
    solution_code = a,
    is_correct = TRUE
  )

  expect_grade_code(
    user_code = d,
    solution_code = a,
    is_correct = TRUE
  )

  expect_grade_code(
    user_code = a,
    solution_code = d,
    is_correct = TRUE
  )
})

test_that("Returns nothing when no solution code is provided", {

  expect_graded(
    grade_code()(list2env(list(.user_code = "1"))),
    is_correct = FALSE,
    msg = "No exercise solution provided. Defaulting to _incorrect_"
  )
})

test_that("Returns intelligent error when no user code", {
  expect_graded(
    grade_code()(emptyenv()),
    is_correct = FALSE,
    msg = "I didn't receive your code. Did you write any?"
  )
})

test_that("Spot differences when pipes are involved", {

  select <- function(df, x) {
    df[[1]]
  }
  filter <- subset
  arrange <- function(df, ...) {
    df
  }

  pipe <- expression(1:10 %>% mean(na.rm = TRUE) %>% log(base = 10)) # nolint
  func <- expression(log(1:10 %>% mean(na.rm = TRUE), base = 10))
  func1 <- expression(log(mean(1:10, na.rm = TRUE), base = 10))
  pipe3 <- expression(iris %>% lm(Sepal.Length ~ Sepal.Width, data = .))
  func3 <- expression(lm(Sepal.Length ~ Sepal.Width, data = iris))

  expect_grade_code(user_code = func,  solution_code = pipe,  is_correct = TRUE)
  expect_grade_code(user_code = func1, solution_code = pipe,  is_correct = TRUE)
  expect_grade_code(user_code = pipe,  solution_code = func,  is_correct = TRUE)
  expect_grade_code(user_code = pipe,  solution_code = func1, is_correct = TRUE)
  expect_grade_code(user_code = pipe,  solution_code = pipe,  is_correct = TRUE)
  expect_grade_code(user_code = func,  solution_code = func1, is_correct = TRUE)
  expect_grade_code(user_code = func1, solution_code = func1, is_correct = TRUE)
  expect_grade_code(user_code = func3, solution_code = pipe3, is_correct = TRUE)
  expect_grade_code(user_code = pipe3, solution_code = func3, is_correct = TRUE)
  expect_grade_code(user_code = pipe3, solution_code = pipe3, is_correct = TRUE)

})

test_that("Spots differences in long calls", {
  # original discussion here:
  # https://github.com/rstudio-education/grader/issues/28

  testthat::skip_if_not_installed("tidyr")

  expect_grade_code(
    user_code = expression(tidyr::gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = TRUE)), # nolint
    solution_code = expression(tidyr::gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = FALSE)), # nolint
    is_correct = FALSE,
    msg = "I expected `na.rm = FALSE` where you wrote `na.rm = TRUE`"
  )

  expect_grade_code(
    user_code = expression(tidyr::gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = TRUE)), # nolint
    solution_code = expression(tidyr::gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = TRUE)), # nolint
    is_correct = TRUE
  )
})

test_that("glue_pipe message returns unpiped text", {
  user_code <- "x %>% a() %>% b() %>% c()"
  expect_grade_code(
    user_code = user_code,
    solution_code = "b(a(x))",
    is_correct = FALSE,
    glue_pipe = "{unpipe_all_str(.user)}",
    msg = unpipe_all_str(user_code)
  )
  
  with_options(
    list(gradethis.glue_pipe = gradethis_default_options$gradethis_glue_pipe),
    expect_grade_code(
      user_code = user_code,
      solution_code = "b(a(x))",
      is_correct = FALSE,
      msg = glue_message(gradethis_default_options$gradethis_glue_pipe, .user = user_code, .message = "")
    )
  )
})