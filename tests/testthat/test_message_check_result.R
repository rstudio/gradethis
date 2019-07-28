context("Check test_result messages")

expect_message <- function(x, message, correct) {
  expect_s3_class(x, "grader_graded")
  expect_equal(x$correct, correct)
  expect_true(grepl(message, paste0(x$message, collapse = ""), fixed = TRUE))
}

test_that("Correct messages without random praise", {
    glue_correct_no_praise <- "{ .message } { .correct }"

    expect_message(
        check_result(
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
        check_result(
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
        check_result(
            pass_if(~ .result == 5, "Only a pass_if message."),
            grader_args = list(),
            learnr_args = list(last_value = 5, envir_prep = new.env()),
            glue_correct = glue_correct_no_praise
        ),
        message = "Only a pass_if message.",
        correct = TRUE
    )

    expect_message(
        check_result(
            pass_if(~ .result == 5),
            grader_args = list(),
            learnr_args = list(last_value = 5, envir_prep = new.env()),
            glue_correct = glue_correct_no_praise
        ),
        message = "",
        correct = TRUE
    )
})

# nolint start
# test_that("Incorrect messages no match pass_if", {
#     set.seed(525600)
#     expect_message(
#         check_result(
#             pass_if(~ .result == 42),
#             grader_args = list(),
#             learnr_args = list(last_value = 5, envir_prep = new.env())
#         ),
#         message = "Try it again. Perseverence is the key to success.", # random praise only
#         correct = FALSE
#     )

#     set.seed(42)
#     expect_message(
#         check_result(
#             pass_if(~ .result == 42, message = "random+pass_if"),
#             grader_args = list(),
#             learnr_args = list(last_value = 5, envir_prep = new.env())
#         ),
#         message = "Please try again.",
#         correct = FALSE
#     )

#     expect_message(
#         check_result(
#             pass_if(~ .result == 42, message = "This does nothing."),
#             grader_args = list(),
#             learnr_args = list(last_value = 5, envir_prep = new.env()),
#             incorrect = "hello"
#         ),
#         message = "hello",
#         correct = FALSE
#     )

#     expect_message(
#         check_result(
#             pass_if(~ .result == 42),
#             grader_args = list(),
#             learnr_args = list(last_value = 5, envir_prep = new.env()),
#             incorrect = "correct with no pass_if message"
#         ),
#         message = "correct with no pass_if message",
#         correct = FALSE
#     )
# })

# test_that("Incorrect messages match fail_if", {
#     set.seed(525600)
#     expect_message(
#         check_result(
#             pass_if(~ .result == 42),
#             fail_if(~ .result == 5),
#             grader_args = list(),
#             learnr_args = list(last_value = 5, envir_prep = new.env())
#         ),
#         message = "Try it again. Perseverence is the key to success.", # random praise only
#         correct = FALSE
#     )

#     set.seed(42)
#     expect_message(
#         check_result(
#             pass_if(~ .result == 42, message = "random+pass_if"),
#             fail_if(~ .result == 5, message = "random+fail_if"),
#             grader_args = list(),
#             learnr_args = list(last_value = 5, envir_prep = new.env())
#         ),
#         message = "Please try again.",
#         correct = FALSE
#     )

#     expect_message(
#         check_result(
#             pass_if(~ .result == 42, message = "This does nothing."),
#             fail_if(~ .result == 5, message = "This does nothing."),
#             grader_args = list(),
#             learnr_args = list(last_value = 5, envir_prep = new.env()),
#             incorrect = "hello"
#         ),
#         message = "hello",
#         correct = FALSE
#     )

#     expect_message(
#         check_result(
#             pass_if(~ .result == 42),
#             fail_if(~ .result == 5),
#             grader_args = list(),
#             learnr_args = list(last_value = 5, envir_prep = new.env()),
#             incorrect = "incorrect with no fail_if message"
#         ),
#         message = "incorrect with no fail_if message",
#         correct = FALSE
#     )
# })
# nolint end
