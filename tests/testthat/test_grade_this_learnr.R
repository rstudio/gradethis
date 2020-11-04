context("Check grade learnr")

expect_grade_this_learnr <- function(x) {
  checkmate::expect_names(names(x), identical.to = c("message", "correct", "type", "location"))
  checkmate::expect_string(x$message, null.ok = TRUE)
  checkmate::expect_logical(x$correct, null.ok = FALSE, len = 1)
  checkmate::expect_string(x$type, null.ok = FALSE)
  checkmate::expect_choice(x$type, choices = c("warning", "success", "error", "info"))
  testthat::expect_equal(x$location, "append")
}

expect_correct_grade_this_learnr <- function(x, message = NULL) {
  expect_grade_this_learnr(x)
  expect_true(x$correct)
  expect_equal(x$type, "success")
  expect_match(x$message, message, fixed = TRUE)
}

expect_wrong_grade_this_learnr <- function(x, message = NULL, type = "error") {
  expect_grade_this_learnr(x)
  expect_false(x$correct)
  expect_equal(x$type, type)
  expect_match(x$message, message, fixed = TRUE)
}

test_grade_this_learnr <- function(
  user_code,
  check_code = "",
  prep_code = "",
  solution_code = NULL,
  ...,
  correct,
  msg,
  msg_type = "error"
) {
  envir_prep <- new.env(parent = .GlobalEnv)
  eval(parse(text = prep_code), envir = envir_prep)

  envir_result <- new.env(parent = envir_prep)
  last_value <- try(eval(parse(text = user_code), envir = envir_result), silent = TRUE)

  ret <- grade_this_learnr(
    label = "test",
    user_code = user_code,
    solution_code = solution_code,
    check_code = check_code,
    envir_result = envir_result,
    evaluate_result = "ignore",
    envir_prep = envir_prep,
    last_value = last_value
  )

  if (isTRUE(correct)) {
    expect_correct_grade_this_learnr(ret, msg)
  } else {
    expect_wrong_grade_this_learnr(ret, msg, type = msg_type)
  }
}

test_that("prep environment is used", {
  test_grade_this_learnr(
    correct = TRUE,
    msg = "yes. you did it. Extra!",
    user_code = "4",
    prep_code = "
      extra <- 'Extra!'
    ",
    check_code = "
      grade_this({
        pass_if_equal(4, 'yes. you did it. {extra}')
      })
    "
  )
})

test_that("parser error is used", {
  with_options(
    list(
      exercise.parse.error = function(...) {
        args <- list(...)
        expect_equal(names(args)[1], "parse_error")
        fail("test parse error")
      }
    ),
    {
      test_grade_this_learnr(
        correct = FALSE,
        msg = "test parse error",
        user_code = "4 +"
      )
    }
  )

  with_options(
    list(
      exercise.parse.error = function(...) {
        args <- list(...)
        expect_equal(names(args)[1], "parse_error")
        feedback(fail("test parse error"))
      }
    ),
    {
      test_grade_this_learnr(
        correct = FALSE,
        msg = "test parse error",
        user_code = "4 +"
      )
    }
  )

  with_options(
    list(
      exercise.parse.error = function(...) {
        stop("boom")
      }
    ),
    {
      test_grade_this_learnr(
        correct = FALSE,
        msg = "boom",
        user_code = "4 +"
      )
    }
  )
})

test_that("length 0 user code", {
  test_grade_this_learnr(
    correct = FALSE,
    msg = "I didn't receive your code. Did you write any?",
    msg_type = "info",
    user_code = ""
  )
})

test_that("length 0 solution code", {
  test_grade_this_learnr(
    correct = FALSE,
    msg = "No solution is provided for this exercise.",
    msg_type = "info",
    user_code = "1",
    solution_code = ""
  )
})


test_that("pass / fail in check chunk are caught", {
  testthat::expect_message(
    test_grade_this_learnr(
      correct = FALSE,
      msg = "Uh Oh! Submission prematurely graded. Marking as _incorrect_",
      user_code = "1",
      solution_code = "1",
      check_code = "pass()"
    ),
    "Prematurely graded"
  )
  testthat::expect_message(
    test_grade_this_learnr(
      correct = FALSE,
      msg = "Uh Oh! Submission prematurely graded. Marking as _incorrect_",
      user_code = "1",
      solution_code = "1",
      check_code = "fail()"
    ),
    "Prematurely graded"
  )
})

test_that("check parsing error is caught", {
  testthat::expect_message(
    test_grade_this_learnr(
      correct = FALSE,
      msg = "Uh Oh! Error executing grading code. Marking as _incorrect_",
      user_code = "1",
      solution_code = "1",
      check_code = "4 +"
    ),
    "Error while executing checking `test-check` chunk: "
  )
})

test_that("return value is a function of 1 argument", {
  testthat::expect_message(
    test_grade_this_learnr(
      correct = FALSE,
      msg = "Uh Oh! Unexpected grading behavior. Marking as _incorrect_",
      user_code = "1",
      solution_code = "1",
      check_code = "1"
    ),
    "chunk did not return a function (such as `grade_this`) that accepts 1 argument", fixed = TRUE
  )
  testthat::expect_message(
    test_grade_this_learnr(
      correct = FALSE,
      msg = "Uh Oh! Unexpected grading behavior. Marking as _incorrect_",
      user_code = "1",
      solution_code = "1",
      check_code = "Sys.time"
    ),
    "chunk did not return a function (such as `grade_this`) that accepts 1 argument", fixed = TRUE
  )

  test_grade_this_learnr(
    correct = TRUE,
    msg = "test pass",
    user_code = "1",
    solution_code = "1",
    check_code = "function(...) pass('test pass')"
  )
})

test_that("a grade is given", {
  testthat::expect_message(
    test_grade_this_learnr(
      correct = FALSE,
      msg = "Uh Oh! No feedback given. Marking as _incorrect_",
      user_code = "1",
      solution_code = "1",
      check_code = "function(...) NULL"
    ),
    "chunk did not mark an answer as correct or incorrect", fixed = TRUE
  )
  test_grade_this_learnr(
    correct = TRUE,
    msg = "test pass",
    user_code = "1",
    solution_code = "1",
    check_code = "function(...) pass('test pass')"
  )
  test_grade_this_learnr(
    correct = FALSE,
    msg = "test fail",
    user_code = "1",
    solution_code = "1",
    check_code = "function(...) fail('test fail')"
  )
  test_grade_this_learnr(
    correct = FALSE,
    msg = "boom",
    user_code = "1",
    solution_code = "1",
    check_code = "function(...) stop('boom')"
  )
})
