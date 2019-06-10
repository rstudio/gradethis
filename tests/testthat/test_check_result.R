context("Check Result")

# these tests are largely redundant exercises that have been tested against detect_mistakes()
expect_correct <- function(x) {
  expect_s3_class(x, "grader_graded")
  expect_true(x$correct)
}
expect_message <- function(x, message, correct = FALSE) {
  expect_s3_class(x, "grader_graded")
  expect_equal(x$correct, correct)
  expect_true(grepl(message, paste0(x$message, collapse = ""), fixed = TRUE))
}

test_that("Spots differences in atomics", {

  expect_correct(
    check_result(
      #grader_args = list(user_quo = user), ## would be supplied but not used
      learnr_args = list(last_value = 1),
      result(1, correct = TRUE)
    )
  )

  expect_correct(
    check_result(
      learnr_args = list(last_value = 2/2),
      result(1, correct = TRUE)
    )
  )

  expect_message(
    check_result(
      learnr_args = list(last_value = 3/2),
      result(1, correct = TRUE),
      incorrect = "check failed!"
    ),
    "check failed!"
  )
})


test_that("Gives correct message", {

  # correct
  expect_message(
    check_result(
      learnr_args = list(last_value = 1),
      correct = "{correct} {message}",
      result(1, correct = TRUE, message = "Result 1")
    ),
    "Result 1", TRUE
  )

  # incorrect
  expect_message(
    check_result(
      learnr_args = list(last_value = 1),
      incorrect = "{correct} {message}",
      result(0, correct = TRUE, message = "Result 0"),
      result(1, correct = FALSE, message = "Result 1")
    ),
    "FALSE Result 1", FALSE
  )

  # not found
  expect_message(
    check_result(
      learnr_args = list(last_value = 1),
      incorrect = "found: {ifelse(matched, message, 'Not Found.')}",
      result(0, correct = TRUE, message = "Result 0"),
      result(2, correct = FALSE, message = "Result 1")
    ),
    "found: Not Found.", FALSE
  )
})
