expect_correct <- function(x) {
  expect_s3_class(x, "grader_graded")
  expect_true(x$correct)
}

expect_wrong <- function(x) {
  expect_s3_class(x, "grader_graded")
  expect_false(x$correct)
}

expect_message <- function(x, message) {
  expect_s3_class(x, "grader_graded")
  expect_true(!x$correct)
  expect_true(grepl(message, paste0(x$message, collapse = ""), fixed = TRUE))
}

expect_condi <- function(x) {
    checkmate::expect_names(names(x), identical.to = c("x", "message", "correct", "type"))
    checkmate::expect_character(x$message, null.ok = TRUE)
    checkmate::expect_logical(x$correct, null.ok = FALSE, len = 1)
    checkmate::expect_choice(x$type, choices = c("formula", "function", "value"))
    checkmate::expect_class(x, "grader_condition")
}

expect_condi_correct <- function(x, message = NULL) {
    expect_condi(x)
    expect_equal(x$message, message)
    expect_true(x$correct)
}

expect_condi_error <- function(x, message = NULL) {
    expect_condi(x)
    expect_equal(x$message, message)
    expect_false(x$correct)
}
