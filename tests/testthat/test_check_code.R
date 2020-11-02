context("Check Code")

# these tests are largely redundant exercises that have been tested against detect_mistakes()

test_that("Spots differences in atomics", {

  user <- expression(1)
  solution <- expression(1)

  expect_correct(
    grade_code(grader_args = list(user_quo = user, solution_quo = solution))
  )

  user <- expression(1)
  solution <- expression(2)
  expect_message(
    grade_code(grader_args = list(user_quo = user, solution_quo = solution)),
    wrong_value(this = quote(1), that = quote(2))
  )
})

test_that("Spots differences in names", {

  user <- expression(x)
  solution <- expression(y)
  expect_message(
    grade_code(grader_args = list(user_quo = user, solution_quo = solution)),
    wrong_value(this = quote(x), that = quote(y))
  )

  user <- expression(x)
  solution <- expression(x)
  expect_correct(
    grade_code(grader_args = list(user_quo = user, solution_quo = solution))
  )

  user <- expression(5)
  solution <- expression(y)
  expect_message(
    grade_code(grader_args = list(user_quo = user, solution_quo = solution)),
    wrong_value(this = quote(5), that = quote(y))
  )
})

test_that("Spots differences in calls", {
  a <- expression(vapply(lists, mean, numeric(1), na.rm = TRUE))
  b <- expression(vapply(vecs, mean, numeric(1), na.rm = TRUE))
  c <- expression(vapply(lists, mean, numeric(1)))
  d <- expression(vapply(vecs, mean, numeric(1)))

  expect_correct(
    grade_code(grader_args = list(user_quo = a, solution_quo = a))
  )

  expect_message(
    grade_code(grader_args = list(user_quo = a, solution_quo = b),
               glue_correct = '{ .message } { .correct }',
               glue_incorrect = '{ .message } { .incorrect }'),
    wrong_value(this = quote(lists), that = quote(vecs))
  )

  expect_message(
    grade_code(grader_args = list(user_quo = a, solution_quo = c)),
    surplus_argument(this_call = "vapply()",
                     this_name = "na.rm",
                     this = quote(TRUE))
  )

  expect_message(
    grade_code(grader_args = list(user_quo = c, solution_quo = a))
    ,
    missing_argument(this_call = "vapply()",
                     that_name = "na.rm")
  )
})

test_that("Mentions only first non-matching element", {
  w <- expression(1)
  x <- expression(log(1))
  y <- expression(sqrt(log(2)))
  z <- expression(sqrt(log(1)))

  expect_correct(
    grade_code(grader_args = list(user_quo = w, solution_quo = w))
  )

  expect_message(
    grade_code(grader_args = list(user_quo = w, solution_quo = z)),
    wrong_value(this = quote(1), that = quote(sqrt()))
  )

  expect_message(
    grade_code(grader_args = list(user_quo = x, solution_quo = z)),
    wrong_call(this = quote(log()), that = quote(sqrt()))
  )

  expect_message(
    grade_code(grader_args = list(user_quo = y, solution_quo = z)),
    wrong_value(this = "2", that = quote(1))
  )

})

test_that("Spots differences in argument names", {
  test_fn <<- function(x, y = 1, z = 2, ...) {return(1)}

  a <- expression(test_fn(10, y = 1, z = TRUE))
  b <- expression(test_fn(10, 1, TRUE))
  c <- expression(test_fn(10, w = 1, z = TRUE))

  expect_correct(
    grade_code(grader_args = list(user_quo = a, solution_quo = a))
  )

  expect_correct(
    grade_code(grader_args = list(user_quo = b, solution_quo = a))
  )

  expect_message(
    grade_code(grader_args = list(user_quo = c, solution_quo = a)),
    surplus_argument(this_call = c[[1]],
                     this = 1,
                     this_name = "w")
  )

})

test_that("Ignore differences in argument positions (for non ... arguments)", {
  test_fn <<- function(x, digits = 0){return(1)}
  a <- expression(test_fn(x = pi, digits = 2))
  b <- expression(test_fn(pi, digits = 2))
  c <- expression(test_fn(2, x = pi))
  d <- expression(test_fn(digits = 2, x = pi))

  expect_correct(
    grade_code(grader_args = list(user_quo = b, solution_quo = a))
  )

  expect_correct(
    grade_code(grader_args = list(user_quo = c, solution_quo = a))
  )

  expect_correct(
    grade_code(grader_args = list(user_quo = d, solution_quo = a))
  )

  expect_correct(
    grade_code(grader_args = list(user_quo = a, solution_quo = d))
  )

})

test_that("Returns nothing when no solution code is provided", {

  testthat::expect_null(
    grade_code(grader_args = list(user_quo = expression(5)))
  )

})

test_that("Returns intelligent error when no user code", {
  testthat::expect_error(
    grade_code(grader_args = list(solution_quo = expression(5))),
    "I didn't receive your code. Did you write any?"
  )
})

test_that("Empty user solution messages", {
  grader_args <- list()
  learnr_args <- list()

  testthat::expect_null(grade_code(grader_args = grader_args, learnr_args = learnr_args))
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

  expect_correct(grade_code(grader_args = list(user_quo = func,  solution_quo = pipe)))
  expect_correct(grade_code(grader_args = list(user_quo = func1, solution_quo = pipe)))
  expect_correct(grade_code(grader_args = list(user_quo = pipe,  solution_quo = func)))
  expect_correct(grade_code(grader_args = list(user_quo = pipe,  solution_quo = func1)))
  expect_correct(grade_code(grader_args = list(user_quo = pipe,  solution_quo = pipe)))
  expect_correct(grade_code(grader_args = list(user_quo = func,  solution_quo = func1)))
  expect_correct(grade_code(grader_args = list(user_quo = func1, solution_quo = func1)))
  expect_correct(grade_code(grader_args = list(user_quo = func3, solution_quo = pipe3)))
  expect_correct(grade_code(grader_args = list(user_quo = pipe3, solution_quo = func3)))
  expect_correct(grade_code(grader_args = list(user_quo = pipe3, solution_quo = pipe3)))

})

test_that("Spots differences in long calls", {
  # original discussion here:
  # https://github.com/rstudio-education/grader/issues/28

  testthat::skip_if_not_installed("tidyr")

  user <- expression(tidyr::gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = TRUE)) # nolint
  solution <- expression(tidyr::gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = FALSE)) # nolint
  expect_output(
    cat(detect_mistakes(user = user, solution = solution)),
    "I expected na.rm = FALSE where you wrote na.rm = TRUE",
    fixed = TRUE
  )

  user <- expression(tidyr::gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = TRUE)) # nolint
  solution <- expression(tidyr::gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = TRUE)) # nolint
  expect_equal(detect_mistakes(user = user, solution = solution), NULL)
})
