context("Check test_result messages")

expect_message <- function(x, message, correct) {
    str(x)
  expect_s3_class(x, "grader_graded")
  expect_equal(x$correct, correct)
  expect_true(grepl(message, paste0(x$message, collapse = ""), fixed = TRUE))
}

test_that("Correct messages", {
    set.seed(42)
    expect_message(
        check_result(
            pass_if(~ .result == 5, message = "random+pass_if"),
                grader_args = list(),
                learnr_args = list(last_value = 5, envir_prep = new.env())
        ),
        message = "You should be proud. random+pass_if",
        correct = TRUE
    )

    expect_message(
        check_result(
            pass_if(~ .result == 5, message = "This does nothing."),
                grader_args = list(),
                learnr_args = list(last_value = 5, envir_prep = new.env()),
                correct = "hello"
        ),
        message = "hello",
        correct = TRUE
    )

    expect_message(
        check_result(
            pass_if(~ .result == 5),
                grader_args = list(),
                learnr_args = list(last_value = 5, envir_prep = new.env()),
                correct = "correct with no pass_if message"
        ),
        message = "correct with no pass_if message",
        correct = TRUE
    )
})
