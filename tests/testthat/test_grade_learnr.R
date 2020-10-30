context("Check grade learnr")

expect_grade_learnr <- function(x) {
  checkmate::expect_names(names(x), identical.to = c("message", "correct", "type", "location"))
  checkmate::expect_string(x$message, null.ok = TRUE)
  checkmate::expect_logical(x$correct, null.ok = FALSE, len = 1)
  checkmate::expect_string(x$type, null.ok = FALSE)
  checkmate::expect_choice(x$type, choices = c("warning", "success", "error"))
  testthat::expect_equal(x$location, "append")
}

expect_correct <- function(x, message = NULL) {
  expect_grade_learnr(x)
  expect_true(x$correct)
  expect_equal(x$type, "success")
  expect_equal(x$message, message)
}

expect_wrong <- function(x, message = NULL) {
  expect_grade_learnr(x)
  expect_false(x$correct)
  expect_equal(x$type, "error")
  expect_equal(x$message, message)
}

test_grade_learnr <- function(user_code,
                              check_code,
                              prep_code = "",
                              solution_code = NULL
                              ) {
  envir_prep <- new.env(parent = .GlobalEnv)
  eval(parse(text = prep_code), envir = envir_prep)

  envir_result <- new.env(parent = envir_prep)
  last_value <- eval(parse(text = user_code), envir = envir_result)

  grade_learnr(label = "test",
               user_code = user_code,
               solution_code = solution_code,
               check_code = check_code,
               envir_result = envir_result,
               evaluate_result = list(),
               envir_prep = envir_prep,
               last_value = last_value)
}

test_that("Grade learnr grade_result", {
  expect_correct(
    test_grade_learnr(
      user_code = "4",
      check_code = "grade_result(
          pass_if(~ .result == 4, 'yes. you did it.'),
          correct = 'this other correct message.',
          glue_correct = '{ .message } { .correct }'
        )"
    ),
    "yes. you did it. this other correct message."
  )
})

test_that("Grade learnr check_code", {
  expect_correct(
    test_grade_learnr(
      user_code = "4",
      check_code = "grade_code(correct = 'This works', glue_correct = '{.correct}')",
      solution_code = "4"
    ),
    "This works"
  )

  expect_wrong(
    test_grade_learnr(
      user_code = "exp(log(2))",
      check_code = "grade_code(glue_incorrect = '{.message}')",
      solution_code = "exp(log(1))"
    ),
    "In log(2), I expected 1 where you wrote 2."
  )

  # User code that produces a parsing error should return an incorrect grade (by default)
  parse_error <- grade_learnr(user_code = "function(")
  expect_true(is_feedback(parse_error))
  expect_false(parse_error$correct)
  expect_true(parse_error$type == "error")

  # Can customize the feedback through an exercise.parse.error function
  parse_error_func <- function(parse_error, learnr_args) {
    graded(
      correct = FALSE,
      message = paste(
        "The user code of", shQuote(learnr_args$user_code),
        "produced an parsing error:", conditionMessage(parse_error)
      )
    )
  }
  opts <- options(exercise.parse.error = parse_error_func)
  on.exit(options(opts), add = TRUE)
  expect_equal(
    grade_learnr(user_code = "function("),
    feedback(
      graded(
        correct = FALSE,
        message = "The user code of 'function(' produced an parsing error: <text>:2:0: unexpected end of input\n1: function(\n   ^"
      )
    )
  )
})
