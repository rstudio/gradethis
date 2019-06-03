context("Check Result")

# these tests are largely redundant exercises that have been tested against detect_mistakes()
expect_correct <- function(x) {
  expect_s3_class(x, "grader_result")
  expect_true(x$correct)
}
expect_message <- function(x, message, correct = FALSE) {
  expect_s3_class(x, "grader_result")
  expect_equal(x$correct, correct)
  expect_true(grepl(message, paste0(x$message, collapse = ""), fixed = TRUE))
}

test_that("Spots differences in atomics", {

  user <- quote(1)

  expect_correct(
    check_result(
      user = user,
      results(
        result(1, correct = TRUE)
      )
    )
  )

  user <- quote(2/2)
  expect_correct(
    check_result(
      user = user,
      results(
        result(1, correct = TRUE)
      )
    )
  )

  user <- quote(3/2)
  expect_message(
    check_result(
      user = user,
      results(
        result(1, correct = TRUE)
      ),
      incorrect = "check failed!"
    ),
    "check failed!"
  )
})


test_that("Gives correct message", {

  # empty
  expect_message(
    check_result(
      user = rlang::quo(NULL),
      empty_msg = "NOT FOUND",
      results(
        result(0, correct = TRUE, message = "Result 0"),
        result(1, correct = FALSE, message = "Result 1")
      )
    ),
    "NOT FOUND", FALSE
  )


  user <- quote(1)

  # correct
  expect_message(
    check_result(
      user = user,
      correct = "{correct} {message}",
      results(
        result(1, correct = TRUE, message = "Result 1")
      )
    ),
    "Result 1", TRUE
  )

  # incorrect
  expect_message(
    check_result(
      user = user,
      incorrect = "{correct} {message}",
      results(
        result(0, correct = TRUE, message = "Result 0"),
        result(1, correct = FALSE, message = "Result 1")
      )
    ),
    "FALSE Result 1", FALSE
  )

  # not found
  expect_message(
    check_result(
      user = user,
      incorrect = "found: {rlang::`%||%`(message, '')}",
      results(
        result(0, correct = TRUE, message = "Result 0"),
        result(2, correct = FALSE, message = "Result 1")
      )
    ),
    "found: ", FALSE
  )

})
