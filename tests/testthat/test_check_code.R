context("Check Code")

# these tests are largely redundant exercises that have been tested against detect_mistakes()

test_that("Spots differences in atomics", {

  user <- quote(1)
  solution <- quote(1)

  expect_correct(
    grade_code(grader_args = list(user_quo = user, solution_quo = solution))
  )

  user <- quote(1)
  solution <- quote(2)
  expect_message(
    grade_code(grader_args = list(user_quo = user, solution_quo = solution)),
    wrong_value(this = quote(1), that = quote(2))
  )
})

test_that("Spots differences in names", {

  user <- quote(x)
  solution <- quote(y)
  expect_message(
    grade_code(grader_args = list(user_quo = user, solution_quo = solution))
    ,
    wrong_value(this = quote(x), that = quote(y))
  )

  user <- quote(x)
  solution <- quote(x)
  expect_correct(
    grade_code(grader_args = list(user_quo = user, solution_quo = solution))
  )

  user <- quote(5)
  solution <- quote(y)
  expect_message(
    grade_code(grader_args = list(user_quo = user, solution_quo = solution)),
    wrong_value(this = quote(5), that = quote(y))
  )
})

test_that("Spots differences in calls", {
  a <- quote(vapply(lists, mean, numeric(1), na.rm = TRUE))
  b <- quote(vapply(vecs, mean, numeric(1), na.rm = TRUE))
  c <- quote(vapply(lists, mean, numeric(1)))
  d <- quote(vapply(vecs, mean, numeric(1)))

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
  w <- quote(1)
  x <- quote(log(1))
  y <- quote(sqrt(log(2)))
  z <- quote(sqrt(log(1)))

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
  
  a <- quote(test_fn(10, y = 1, z = TRUE))
  b <- quote(test_fn(10, 1, TRUE))
  c <- quote(test_fn(10, w = 1, z = TRUE))

  expect_correct(
    grade_code(grader_args = list(user_quo = a, solution_quo = a))
  )

  expect_correct(
    grade_code(grader_args = list(user_quo = b, solution_quo = a))
  )

  expect_message(
    grade_code(grader_args = list(user_quo = c, solution_quo = a))
    ,
    surplus_argument(this_call = c,
                     this = 1,
                     this_name = "w")
  )

})

test_that("Ignore differences in argument positions (for non ... arguments)", {
  test_fn <<- function(x, digits = 0){return(1)}
  a <- quote(test_fn(x = pi, digits = 2))
  b <- quote(test_fn(pi, digits = 2))
  c <- quote(test_fn(2, x = pi))
  d <- quote(test_fn(digits = 2, x = pi))

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

test_that("Returns intelligent error when no solution code", {

  testthat::expect_error(
    grade_code(),
    "No solution is provided for this exercise."
  )

})

test_that("Returns intelligent error when no user code", {
  testthat::expect_error(
    grade_code(grader_args = list(solution_quo = quote(5))),
    "I didn't receive your code. Did you write any?"
  )
})

test_that("Empty user solution messages", {
  grader_args <- list()
  learnr_args <- list()

  testthat::expect_error(grade_code(grader_args = grader_args, learnr_args = learnr_args),
                         "No solution is provided for this exercise.")
})

test_that("Spot differences when pipes are involved", {

  select <- function(df, x) {
    df[[1]]
  }
  filter <- subset
  arrange <- function(df, ...) {
    df
  }

  pipe <- quote(1:10 %>% mean(na.rm = TRUE) %>% log(base = 10)) # nolint
  func <- quote(log(1:10 %>% mean(na.rm = TRUE), base = 10))
  func1 <- quote(log(mean(1:10, na.rm = TRUE), base = 10))
  pipe3 <- quote(iris %>% lm(Sepal.Length ~ Sepal.Width, data = .))
  func3 <- quote(lm(Sepal.Length ~ Sepal.Width, data = iris))

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

  user <- quote(tidyr::gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = TRUE)) # nolint
  solution <- quote(tidyr::gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = FALSE)) # nolint
  expect_wrong(
    grade_code(grader_args = list(user_quo = user, solution_quo = solution))
  )

  user <- quote(tidyr::gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = TRUE)) # nolint
  solution <- quote(tidyr::gather(key = key, value = value, new_sp_m014:newrel_f65, na.rm = TRUE)) # nolint
   expect_correct(
    grade_code(grader_args = list(user_quo = user, solution_quo = solution))
  )
})
