expect_correct <- function(x) {
  expect_s3_class(x, "gradethis_graded")
  expect_true(x$correct)
  invisible(x)
}

expect_wrong <- function(x) {
  expect_s3_class(x, "gradethis_graded")
  expect_false(x$correct)
  invisible(x)
}

expect_message <- function(x, message) {
  expect_s3_class(x, "gradethis_graded")
  x_message <- paste0(x$message, collapse = "\n")
  if (!grepl(message, paste0(x$message, collapse = ""), fixed = TRUE)) {
    cat("\nReceived:\n", x$message, "\n")
    cat("Expected:\n", message, "\n")
    testthat::fail("message does not match")
  }
}

expect_condi <- function(x) {
    checkmate::expect_names(names(x), identical.to = c("x", "message", "correct", "type"))
    checkmate::expect_character(x$message, null.ok = TRUE)
    checkmate::expect_logical(x$correct, null.ok = FALSE, len = 1)
    checkmate::expect_choice(x$type, choices = c("formula", "function", "value"))
    checkmate::expect_class(x, "gradethis_condition")
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



eval_this <- function(
  expr,
  user_code,
  solution_code = NULL,
  envir_prep = parent.frame(),
  correct = "valid",
  incorrect = "invalid"
) {
  env <- new.env(parent = envir_prep)
  env$.result <-
    env$.last_value <-
      eval(parse(text = user_code), envir = new.env(parent = env))
  env$.user_code <- user_code
  env$.solution_code <- solution_code
  env$.solution <-
    if (is.null(solution_code)) {
      NULL
    } else {
      eval(parse(text = solution_code), envir = new.env(parent = env))
    }
  env$.envir_prep <- envir_prep

  expr_quo <- rlang::enquo(expr)
  x <- grade_this(!!expr_quo)(env)

  expect_s3_class(x, "gradethis_graded")
  invisible(x)
}

expect_correct_code <- function(
  user_code,
  solution_code,
  envir_prep = parent.frame(),
  correct = "valid",
  incorrect = "invalid"
) {
  env <- new.env(parent = envir_prep)
  env$.user_code <- user_code
  env$.solution_code <- solution_code
  env$.envir_prep <- envir_prep

  x <- grade_this_code(correct, incorrect)(env)
  expect_s3_class(x, "gradethis_graded")
  expect_true(x$correct)
  invisible(x)
}
expect_wrong_code <- function(
  user_code,
  solution_code,
  envir_prep = parent.frame(),
  correct = "Correct",
  incorrect = "{.message}",
  msg = NULL,
  this_code = user_code,
  that_code = solution_code
) {
  env <- new.env(parent = envir_prep)
  env$.user_code <- user_code
  env$.solution_code <- solution_code
  env$.envir_prep <- envir_prep

  x <- grade_this_code(correct, incorrect)(env)
  expect_s3_class(x, "gradethis_graded")
  expect_false(x$correct)

  if (!is.null(msg)) {
    expect_message(x, msg)
  }
  invisible(x)
}
