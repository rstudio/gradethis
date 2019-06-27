context("Check grade learnr")

expect_grade_learnr <- function(x) {
  str(x)
  checkmate::expect_names(names(x), identical.to = c('message', 'correct', 'type', 'location'))
  checkmate::expect_string(x$message, null.ok = TRUE)
  checkmate::expect_logical(x$correct, null.ok = FALSE, len = 1)
  checkmate::expect_string(x$type, null.ok = FALSE)
  checkmate::expect_choice(x$type, choices = c('warning', 'success', 'error'))
  testthat::expect_equal(x$location, 'append')
}

expect_correct <- function(x, message = NULL) {
  expect_grade_learnr(x)
  expect_true(x$correct)
  expect_equal(x$type, 'success')
  expect_equal(x$message, message)
}

expect_error <- function(x, message = NULL) {
  expect_grade_learnr(x)
  expect_false(x$correct)
  expect_equal(x$type, 'error')
  expect_equal(x$message, message)
}

test_grade_learnr <- function(user_code,
                              check_code,
                              prep_code = "",
                              solution_code = NULL#,
                              # envir_result = NULL,
                              # envir_prep = NULL,
                              # last_value = NULL
                              ) {
  envir_prep <- new.env(parent = .GlobalEnv)
  eval(parse(text = prep_code), envir = envir_prep)

  envir_result <- new.env(parent = envir_prep)
  last_value <- eval(parse(text = user_code), envir = envir_result)

  grade_learnr(label = 'test', user_code = user_code, solution_code = solution_code, check_code = check_code,
               envir_result = envir_result,
               evaluate_result = list(), envir_prep = envir_prep, last_value = last_value)
}

test_that("grade learnr", {
  expect_correct(
    test_grade_learnr(
      user_code = '4',
      check_code = "check_code(correct = 'This works')",
      solution_code = '4'
    ),
    'This works'
  )
  
  expect_error(
    test_grade_learnr(
      user_code = 'exp(log(2))',
      check_code = "check_code(incorrect = '{message}')",
      solution_code = 'exp(log(1))'
    ),
    'I expected 1 where you wrote 2.'
  )
  
  expect_correct(
    test_grade_learnr(
      user_code = '4',
      check_code = "check_result(result(4, correct = TRUE), correct = 'This works')",
      solution_code = '4'
    ),
    'This works'
  )
})
