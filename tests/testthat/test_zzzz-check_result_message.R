test_that("Correct messages without random praise", {
  glue_correct_no_praise <- "{ .message } { .correct }"

  expect_grade_result(
    pass_if(~ .result == 5, message = "A pass_if message."),
    correct = "A correct message.",
    glue_correct = glue_correct_no_praise,
    last_value = 5,
    is_correct = TRUE,
    msg = "A pass_if message. A correct message."
  )

  expect_grade_result(
    pass_if(~ .result == 5),
    correct = "Only a correct message.",
    glue_correct = glue_correct_no_praise,
    last_value = 5,
    is_correct = TRUE,
    msg = "Only a correct message."
  )

  expect_grade_result(
    pass_if(~ .result == 5, "Only a pass_if message."),
    glue_correct = glue_correct_no_praise,
    last_value = 5,
    is_correct = TRUE,
    msg = "Only a pass_if message."
  )

  expect_grade_result(
    pass_if(~ .result == 5),
    glue_correct = glue_correct_no_praise,
    last_value = 5,
    is_correct = TRUE
  )
})

test_that("Incorrect messages no match pass_if", {
  glue_incorrect_no_praise <- "{ .message } { .incorrect }"

  expect_grade_result(
    pass_if(~ .result == 42),
    glue_incorrect = glue_incorrect_no_praise,
    last_value = 5,
    is_correct = FALSE
  )

  expect_grade_result(
    pass_if(~ .result == 42, message = "This does nothing (expected)."),
    last_value = 5,
    is_correct = FALSE
  )

  expect_grade_result(
    pass_if(~ .result == 42, message = "This does nothing (expected)."),
    incorrect = "hello",
    last_value = 5,
    is_correct = FALSE,
    msg = "hello"
  )

  expect_grade_result(
    pass_if(~ .result == 42),
    incorrect = "incorrect with no pass_if message",
    last_value = 5,
    is_correct = FALSE,
    msg = "incorrect with no pass_if message"
  )
})

test_that("Incorrect messages match fail_if", {

  expect_grade_result(
    pass_if(~ .result == 42),
    fail_if(~ .result == 5),
    last_value = 5,
    is_correct = FALSE
  )

  expect_grade_result(
    pass_if(~ .result == 42, message = "No match here."),
    fail_if(~ .result == 5, message = "Found an incorrect match."),
    last_value = 5,
    is_correct = FALSE,
    msg = "Found an incorrect match."
  )

  expect_grade_result(
    pass_if(~ .result == 42, message = "No match here."),
    fail_if(~ .result == 5, message = "Found an incorrect match."),
    incorrect = "hello",
    last_value = 5,
    is_correct = FALSE,
    msg = "Found an incorrect match. hello"
  )

  expect_grade_result(
    pass_if(~ .result == 42),
    fail_if(~ .result == 5),
    incorrect = "incorrect with no fail_if message.",
    last_value = 5,
    is_correct = FALSE,
    msg = "incorrect with no fail_if message."
  )
})
