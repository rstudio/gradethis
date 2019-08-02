context("Test Result")

# these tests are largely redundant exercises that have been tested against detect_mistakes()

expect_message <- function(x, message, correct = FALSE) {
  expect_s3_class(x, "grader_graded")
  expect_equal(x$correct, correct)
  expect_true(grepl(message, paste0(x$message, collapse = ""), fixed = TRUE))
}

test_that("Spots differences in atomics", {

  user <- quote(1)

  expect_message(
    test_result(
      grader_args = list(user_quo = user),
      checkmate::expect_numeric
    ),
    "1/1", TRUE
  )

  expect_message(
    test_result(
      grader_args = list(user_quo = user),
      checkmate::expect_numeric,
      checkmate::expect_character
    ),
    "1/2", FALSE
  )

  expect_message(
    test_result(
      grader_args = list(user_quo = user),
      checkmate::expect_numeric,
      test("test: is character", checkmate::expect_character)
    ),
    "test: is character", FALSE
  )
})


test_that("Gives correct message", {

  # empty
  # no longer testing for empty user code
  # expect_message(
  #   test_result(
  #     grader_args = list(user_quo = rlang::quo(NULL)),
  #     empty_msg = "NOT FOUND",
  #     checkmate::expect_numeric
  #   ),
  #   "NOT FOUND", FALSE
  # )


  user <- quote(1)

  # correct
  expect_message(
    test_result(
      grader_args = list(user_quo = user),
      correct = "{num_correct}-{num_total}",
      checkmate::expect_numeric
    ),
    "1-1", TRUE
  )

  # incorrect
  expect_message(
    test_result(
      grader_args = list(user_quo = user),
      incorrect = "{num_correct}-{num_total}",
      checkmate::expect_numeric,
      checkmate::expect_character
    ),
    "1-2", FALSE
  )

})
