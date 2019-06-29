context("Check Result Condi")

expect_correct <- function(x) {
  expect_s3_class(x, "grader_graded")
  expect_true(x$correct)
}

expect_error <- function(x) {
  expect_s3_class(x, "grader_graded")
  expect_false(x$correct)
}

expect_message <- function(x, message, correct = FALSE) {
  expect_s3_class(x, "grader_graded")
  expect_equal(x$correct, correct)
  expect_true(grepl(message, paste0(x$message, collapse = ""), fixed = TRUE))
}

test_that("Spots differences in atomics", {

  expect_correct(
    check_result(
        pass_if(~ .result == 5, "This is a correct message"),
        grader_args = list(solution_quo = quote(5)),
        learnr_args = list(envir_prep = new.env())
    )
  )

  expect_error(
    check_result(
        pass_if(~ .result == 5, "This is a wrong answer!"),
        grader_args = list(solution_quo = quote(100000)),
        learnr_args = list(envir_prep = new.env())
    )
  )

  expect_error(
    check_result(
        fail_if(~ .result == 5, "You were supposed to do this other thing!"),
        grader_args = list(solution_quo = quote(5)),
        learnr_args = list(envir_prep = new.env())
    )
  )
})
