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


expect_grade_result <- function(
  ...,
  last_value,
  envir_prep = new.env(parent = parent.frame()),
  is_correct,
  msg = NULL
) {
  user_code <- deparse_to_string(last_value)
  check_env <- create_learnr_env(user_code, solution_code = NULL, envir_prep)
  grader <- grade_result(...)
  grade <- grader(check_env)
  expect_graded(grade, is_correct = is_correct, msg = msg)
}
expect_grade_result_strict <- function(
  ...,
  last_value,
  envir_prep = new.env(parent = parent.frame()),
  is_correct,
  msg = NULL
) {
  user_code <- deparse_to_string(last_value)
  check_env <- create_learnr_env(user_code, solution_code = NULL, envir_prep)
  grade <- grade_result_strict(...)(check_env)
  expect_graded(grade, is_correct = is_correct, msg = msg)
}


expect_grade_code <- function(
  ...,
  user_code,
  solution_code,
  envir_prep = new.env(parent = parent.frame()),
  is_correct,
  msg = NULL
) {
  check_env <- create_learnr_env(user_code, solution_code, envir_prep, eval = FALSE)
  grader <- grade_code(...)
  grade <- grader(check_env)
  expect_graded(grade, is_correct = is_correct, msg = msg)
}

expect_grade_this <- function(
  expr,
  user_code,
  solution_code = NULL,
  envir_prep = new.env(parent = parent.frame()),
  is_correct,
  msg = NULL
) {
  env <- create_learnr_env(user_code, solution_code, envir_prep)

  expr_quo <- rlang::enquo(expr)
  grader <- grade_this(!!expr_quo)
  grade <- grader(env)

  expect_graded(grade, is_correct = is_correct, msg = msg)
}

expect_this_code <- function(
  user_code,
  solution_code,
  envir_prep = new.env(parent = parent.frame()),
  correct = "valid",
  incorrect = "{.message}",
  is_correct,
  msg = NULL,
  allow_partial_matching = TRUE
) {
  env <- create_learnr_env(user_code, solution_code, envir_prep, eval = FALSE)
  grade <- grade_this_code(correct, incorrect, allow_partial_matching = allow_partial_matching)(env)
  expect_graded(grade, is_correct = is_correct, msg = msg)
}

expect_graded <- function(
  grade,
  is_correct,
  msg = NULL
) {
  expect_s3_class(grade, "gradethis_graded")
  if (identical(is_correct, logical(0))) {
    expect_equal(grade$correct, logical(0))
  } else if (isTRUE(is_correct)) {
    expect_true(grade$correct)
  } else {
    expect_false(grade$correct)
  }
  if (!is.null(msg)) {
    if (is.character(msg)) {
      expect_match(grade$message, msg, fixed = TRUE)
    } else {
      expect_equal(grade$message, msg)
    }
  }
  invisible(grade)
}

expect_feedback <- function(
  feedback,
  is_correct,
  type = NULL,
  location = NULL,
  msg = NULL
) {
  if (is_graded(feedback)) {
    feedback <- feedback(feedback)
  }
  
  expect_s3_class(feedback, "gradethis_feedback")
  if (identical(is_correct, logical(0))) {
    expect_equal(feedback$correct, logical(0))
  } else if (isTRUE(is_correct)) {
    expect_true(feedback$correct)
  } else {
    expect_false(feedback$correct)
  }
  if (!is.null(type)) {
    expect_equal(feedback$type, type)
  }
  if (!is.null(location)) {
    expect_equal(feedback$location, location)
  }
  if (!is.null(msg)) {
    if (is.character(msg)) {
      expect_match(feedback$message, msg, fixed = TRUE)
    } else {
      expect_equal(feedback$message, msg)
    }
  }
  invisible(feedback)
}


create_learnr_env <- function(user_code, solution_code = NULL, envir_prep, eval = TRUE) {
  env <- new.env(parent = envir_prep)
  env$.envir_prep <- envir_prep
  env$.envir_result <- new.env(parent = envir_prep)
  env$.user_code <- as.character(user_code)
  env$.solution_code <- as.character(solution_code)
  if (isTRUE(eval)) {
    env$.result <-
      env$.last_value <-
        eval(parse(text = user_code), envir = env$.envir_result)
    env$.solution <-
      if (is.null(solution_code)) {
        NULL
      } else {
        eval(parse(text = solution_code), envir = new.env(parent = envir_prep))
      }
  }

  env
}

expect_exercise_checker <- function(
  user_code,
  check_code = "function(...) stop('boom')",
  prep_code = "",
  solution_code = NULL,
  ...,
  is_correct,
  msg,
  msg_type = NULL,
  msg_fixed = TRUE
) {
  envir_prep <- new.env(parent = .GlobalEnv)
  eval(parse(text = prep_code), envir = envir_prep)

  envir_result <- new.env(parent = envir_prep)
  last_value <- try(eval(parse(text = user_code), envir = envir_result), silent = TRUE)

  feedback <- gradethis_exercise_checker(
    label = "test",
    user_code = user_code,
    solution_code = solution_code,
    check_code = check_code,
    envir_result = envir_result,
    evaluate_result = "ignore",
    envir_prep = envir_prep,
    last_value = last_value
  )

  checkmate::expect_names(names(feedback), identical.to = c("message", "correct", "type", "location"))
  checkmate::expect_string(feedback$message, null.ok = TRUE)
  checkmate::expect_logical(feedback$correct, null.ok = FALSE, len = 1)
  checkmate::expect_string(feedback$type, null.ok = FALSE)
  checkmate::expect_choice(feedback$type, choices = c("warning", "success", "error", "info"))
  testthat::expect_equal(feedback$location, "append")

  expect_equal(feedback$correct, isTRUE(is_correct))
  msg_type <- msg_type %||% (if (isTRUE(is_correct)) "success" else "error")
  expect_equal(feedback$type, msg_type)
  
  msg <- message_md(msg)
  expect_match(feedback$message, msg, fixed = msg_fixed)
  
  invisible(feedback)
}

with_gradethis_setup <- function(expr, ...) {
  old_opts <- gradethis_setup(...)
  on.exit(options(old_opts), add = TRUE)
  force(expr)
}
