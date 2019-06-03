context("Test Result")

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

  expect_message(
    test_result(
      user = user,
      tests(
        checkmate::expect_numeric
      )
    ),
    "1/1", TRUE
  )

  expect_message(
    test_result(
      user = user,
      tests(
        checkmate::expect_numeric,
        checkmate::expect_character
      )
    ),
    "1/2", FALSE
  )

  expect_message(
    test_result(
      user = user,
      tests(
        checkmate::expect_numeric,
        test("test: is character", checkmate::expect_character)
      )
    ),
    "test: is character", FALSE
  )
})


test_that("Gives correct message", {

  # empty
  expect_message(
    test_result(
      user = rlang::quo(NULL),
      empty_msg = "NOT FOUND",
      tests(
        checkmate::expect_numeric
      )
    ),
    "NOT FOUND", FALSE
  )


  user <- quote(1)

  # correct
  expect_message(
    test_result(
      user = user,
      correct = "{num_correct}-{num_total}",
      tests(
        checkmate::expect_numeric
      )
    ),
    "1-1", TRUE
  )

  # incorrect
  expect_message(
    test_result(
      user = user,
      incorrect = "{num_correct}-{num_total}",
      tests(
        checkmate::expect_numeric,
        checkmate::expect_character
      )
    ),
    "1-2", FALSE
  )

})
