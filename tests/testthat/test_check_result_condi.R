context("Check Result Condi")

expect_correct <- function(x) {
  expect_s3_class(x, "grader_graded")
  expect_true(x$correct)
}

expect_wrong <- function(x) {
  expect_s3_class(x, "grader_graded")
  expect_false(x$correct)
}

expect_message <- function(x, message, correct = FALSE) {
  expect_s3_class(x, "grader_graded")
  expect_equal(x$correct, correct)
  expect_true(grepl(message, paste0(x$message, collapse = ""), fixed = TRUE))
}

test_that("Provide a passing solution. Give the students a fighting chance!", {
  testthat::expect_error(
    check_result(
      grader_args = list(),
      learnr_args = list(last_value = quote(5), envir_prep = new.env())
    )
  )

  testthat::expect_error(
    check_result(
      fail_if(~ .result == 5, "You were supposed to do this other thing!"),
      grader_args = list(),
      learnr_args = list(last_value = quote(5), envir_prep = new.env())
    )
  )

  testthat::expect_error(
    check_result(
      fail_if(~ .result == 5, "You were supposed to do this other thing!"),
      fail_if(~ .result == 10, "You were supposed to do this other thing!"),
      grader_args = list(),
      learnr_args = list(last_value = quote(5), envir_prep = new.env())
    )
  )
})

test_that("Spots differences in atomics -- formuula", {

  expect_correct(
    check_result(
        pass_if(~ .result == 5, "This is a correct message"),
        grader_args = list(),
        learnr_args = list(last_value = quote(5), envir_prep = new.env())
    )
  )

  expect_wrong(
    check_result(
        pass_if(~ .result == 5, "This is a wrong answer!"),
        grader_args = list(),
        learnr_args = list(last_value = quote(100000), envir_prep = new.env())
    )
  )

  expect_wrong(
    check_result(
        fail_if(~ .result == 5, "You were supposed to do this other thing!"),
        pass_if(~ TRUE, "should never reach here"),
        grader_args = list(),
        learnr_args = list(last_value = quote(5), envir_prep = new.env())
    )
  )
})

test_that("Spots differences in atomics -- function", {

  expect_correct(
    check_result(
        pass_if(function(x) x == 5, "This is a correct message"),
        learnr_args = list(last_value = quote(5))
    )
  )

  expect_wrong(
    check_result(
        pass_if(function(x) x == 5, "This is a wrong answer!"),
        learnr_args = list(last_value = quote(100000))
    )
  )

  expect_wrong(
    check_result(
        fail_if(function(x) x == 5, "You were supposed to do this other thing!"),
        pass_if(~ TRUE, "should never reach here"),
        learnr_args = list(last_value = quote(5))
    )
  )
})

test_that("Spots differences in atomics -- value", {

  expect_correct(
    check_result(
        pass_if(5, "This is a correct message"),
        learnr_args = list(last_value = quote(5))
    )
  )

  expect_wrong(
    check_result(
        pass_if(5, "This is a wrong answer!"),
        learnr_args = list(last_value = quote(100000))
    )
  )

  expect_wrong(
    check_result(
        fail_if(5, "You were supposed to do this other thing!"),
        pass_if(~ TRUE, "should never reach here"),
        learnr_args = list(last_value = quote(5))
    )
  )
})
