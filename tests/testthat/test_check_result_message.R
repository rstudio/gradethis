context("Check grade_conditions messages")

expect_message <- function(x, message, correct) {
  expect_s3_class(x, "grader_graded")
  expect_equal(x$correct, correct)
  expect_true(grepl(message, paste0(x$message, collapse = ""), fixed = TRUE))
}

test_that("Correct messages without random praise", {
    glue_correct_no_praise <- "{ .message } { .correct }"

    expect_message(
        grade_result(
            pass_if(~ .result == 5, message = "A pass_if message."),
            correct = "A correct message.",
            grader_args = list(),
            learnr_args = list(last_value = 5, envir_prep = new.env()),
            glue_correct = glue_correct_no_praise
        ),
        message = "A pass_if message. A correct message.",
        correct = TRUE
    )

    expect_message(
        grade_result(
            pass_if(~ .result == 5),
            correct = "Only a correct message.",
            grader_args = list(),
            learnr_args = list(last_value = 5, envir_prep = new.env()),
            glue_correct = glue_correct_no_praise
        ),
        message = "Only a correct message.",
        correct = TRUE
    )

    expect_message(
        grade_result(
            pass_if(~ .result == 5, "Only a pass_if message."),
            grader_args = list(),
            learnr_args = list(last_value = 5, envir_prep = new.env()),
            glue_correct = glue_correct_no_praise
        ),
        message = "Only a pass_if message.",
        correct = TRUE
    )

    expect_message(
        grade_result(
            pass_if(~ .result == 5),
            grader_args = list(),
            learnr_args = list(last_value = 5, envir_prep = new.env()),
            glue_correct = glue_correct_no_praise
        ),
        message = "",
        correct = TRUE
    )
})

test_that("Incorrect messages no match pass_if", {
    glue_incorrect_no_praise <- "{ .message } { .incorrect }"

    expect_message(
        grade_result(
            pass_if(~ .result == 42),
            grader_args = list(),
            learnr_args = list(last_value = 5, envir_prep = new.env()),
            glue_incorrect = glue_incorrect_no_praise
        ),
        message = "",
        correct = FALSE
    )

    expect_message(
        grade_result(
            pass_if(~ .result == 42, message = "This does nothing (expected)."),
            grader_args = list(),
            learnr_args = list(last_value = 5, envir_prep = new.env())
        ),
        message = "",
        correct = FALSE
    )

    expect_message(
        grade_result(
            pass_if(~ .result == 42, message = "This does nothing (expected)."),
            grader_args = list(),
            learnr_args = list(last_value = 5, envir_prep = new.env()),
            incorrect = "hello"
        ),
        message = "hello",
        correct = FALSE
    )

    expect_message(
        grade_result(
            pass_if(~ .result == 42),
            grader_args = list(),
            learnr_args = list(last_value = 5, envir_prep = new.env()),
            incorrect = "incorrect with no pass_if message"
        ),
        message = "incorrect with no pass_if message",
        correct = FALSE
    )
})

test_that("Incorrect messages match fail_if", {
    expect_message(
        grade_result(
            pass_if(~ .result == 42),
            fail_if(~ .result == 5),
            grader_args = list(),
            learnr_args = list(last_value = 5, envir_prep = new.env())
        ),
        message = "",
        correct = FALSE
    )

    expect_message(
        grade_result(
            pass_if(~ .result == 42, message = "No match here."),
            fail_if(~ .result == 5, message = "Found an incorrect match."),
            grader_args = list(),
            learnr_args = list(last_value = 5, envir_prep = new.env())
        ),
        message = "Found an incorrect match.",
        correct = FALSE
    )

    expect_message(
        grade_result(
            pass_if(~ .result == 42, message = "No match here."),
            fail_if(~ .result == 5, message = "Found an incorrect match."),
            grader_args = list(),
            learnr_args = list(last_value = 5, envir_prep = new.env()),
            incorrect = "hello"
        ),
        message = "Found an incorrect match. hello",
        correct = FALSE
    )

    expect_message(
        grade_result(
            pass_if(~ .result == 42),
            fail_if(~ .result == 5),
            grader_args = list(),
            learnr_args = list(last_value = 5, envir_prep = new.env()),
            incorrect = "incorrect with no fail_if message."
        ),
        message = "incorrect with no fail_if message.",
        correct = FALSE
    )
})
