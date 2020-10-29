# graded_expr <- function(expr, correct, message = NULL) {
#   chkm8_single_character(message)
#   checkmate::expect_logical(correct, any.missing = FALSE, len = 1, null.ok = FALSE)
#
#   structure(
#     list(
#       expr = expr,
#       correct = correct,
#       message = message %||% ""
#     ),
#     class = "gradethis_graded_expr"
#   )
# }
#
# is_graded_expr <- function(x) {
#   inherits(x, "gradethis_graded_expr")
# }
#


#' @rdname grade_result
#' @export
#' @param x A formula, function, or value, that returns `TRUE` or `FALSE`.
#'    When comparing objects that are greater than length 1
#'    (e.g., vectors, dataframes, matricies, etc)
#'    A boolean vector will be returned if the user uses `==`, not a single boolean value.
#'    `gradethis` will run the vector through
#'     `all(..., na.rm = TRUE)` to check for the boolean value.
#'    It is advised that the user use `identical()` instead of `==` in this case.
#' @param message character string for message returned (usually passed in from
#'    [pass_if()] or [fail_if()].
#' @param correct logical whether the condition is the correct answer.
condition_throw <- function(expr, msg, correct, env = parent.frame()) {

  expr_quo <- rlang::as_quosure(expr, env = env)
  expr_code <- rlang::quo_squash(expr_quo)
  expr_val <- rlang::eval_tidy(expr_quo)
  expr_is_true <- isTRUE(expr_val)

  # the expression did not evaluate to TRUE, so do not signal the condition
  if (!expr_is_true) {
    return(invisible())
  }

  correct <- isTRUE(correct)
  class_val <- c(
    if (correct) {
      "gradethis_condition_correct"
    } else {
      "gradethis_condition_incorrect"
    },
    "gradethis_condition",
    "condition"
  )

  condition_obj <- structure(
    list(
      # expr = expr_code,
      message = msg %||% "",
      correct = correct
    ),
    class = class_val
  )

  # throw condition object
  message(condition_obj)
}

#' @rdname grade_result
#' @export
is_gradethis_condition <- function(x) {
  inherits(x, "gradethis_condition")
}

#' @rdname grade_result
#' @export
pass_if_throw <- function(expr, msg = "Correct!", env = parent.frame()) {
  expr_quo <- rlang::as_quosure(expr, env)
  condition_throw(expr = expr_quo, msg = msg, correct = TRUE, env = env)
}

#' @rdname grade_result
#' @export
fail_if_throw <- function(expr, msg = "Booooo!", env = parent.frame()) {
  expr_quo <- rlang::as_quosure(expr, env)
  condition_throw(expr = expr_quo, msg = msg, correct = FALSE, env = env)
}

#' @rdname grade_result
#' @export
pass <- function(msg = "Correct!", env = parent.frame()) {
  pass_if_throw(TRUE, msg = msg, env = env)
}
#' @rdname grade_result
#' @export
fail <- function(msg = "Booooo!", env = parent.frame()) {
  fail_if_throw(TRUE, msg = msg, env = env)
}


## (if the condition makes it to the top level...)
## print method to override a gradethis condition to have a "friendly" print method
#' @export
conditionMessage.gradethis_condition <- function(c) {
  condition_obj <- c

  if (condition_obj$correct) {
    paste0("gradethis - Correct: ", as.character(condition_obj$message))
  } else {
    paste0("gradethis - Incorrect: ", as.character(condition_obj$message))
  }
}




#' @export
capture_errors <- function(expr, error = error) {
  if (is.null(error)) {
    error <- function(e, that_env) {
      print("turning error into failure")
      str(e)
      ret <- fail(e$message)
      rlang::return_from(that_env)
    }
  }
  this_env <- rlang::current_env()
  withCallingHandlers(
    error = function(e) {
      error(e, this_env)
    },
    expr
  )
}

## This function solves the situation of trying to execute a "single line of code" code block
## Because the gradethis condition is "thrown", the code block will return when the first condition is received
## Ex:
# {
#   pass_if_throw(1)
#   pass_if_throw(2)
#   pass_if_throw(3)
# }
#' @export
capture_gradethis_conditions <- function(expr) {
  this_env <- rlang::current_env()
  withCallingHandlers(
    gradethis_condition = function(gc_obj) {
      rlang::return_from(this_env, gc_obj)
    },
    expr
  )
  # tryCatch(expr,
  #   # return object right away
  #   gradethis_condition = function(gc_obj) {
  #     gc_obj
  #   }
  # )
}
ignore_gradethis_conditions <- function(expr) {
  withCallingHandlers(
    gradethis_condition = function(gc_obj) {
      # do nothing
    },
    expr
  )
}

# helper method to evaluate an expr
# will capture errors and turn them into `failure("message")`
#
#' @export
eval_gradethis_expr <- function(expr, error = NULL) {
  capture_gradethis_conditions({
    capture_errors(expr, error = error)
  })
}





#' @export
any_success <- function(
  ...,
  env = parent.frame(),
  no_success_msg = "default wrong any_success message"
  # no_success_msg = "{ .message } { .incorrect }"
  # no_success_msg = getOption("gradethis_glue_incorrect")
) {

  items <- rlang::quos(...)
  # print(pryr::rls(env = rlang::quo_get_env(items[[1]])))
  for (item in items) {
    ret <- eval_gradethis_expr({
      rlang::eval_tidy(rlang::quo_get_expr(item), env = env)
    })

    if (is_gradethis_condition(ret)) {
      return(message(ret))
    }
  }

  glue_env <- learnr::duplicate_env(env)

  # no success_found
  failure(glue_with_env(glue_env, no_success_msg))
}


#' @export
all_success <- function(
  ...,
  env = parent.frame(),
  # no_success_msg = "{ .num_correct }/{ .num_total } correct!
  success_msg = getOption("gradethis_glue_correct_test"),
  no_success_msg = getOption("gradethis_glue_incorrect_test")
) {
  items <- rlang::quos(...)
  str(items)
  rets <- lapply(
    items,
    function(item) {
      eval_gradethis_expr({
        rlang::eval_tidy(rlang::quo_get_env(item), env = env)

        # TODO
        # if this item does not throw a gradethis_condition, then it's a failure?
        success("default success message")
      })
    }
  )

  glue_env <- learnr::duplicate_env(env)
  glue_env$.num_correct <- sum(vapply(rets, function(x) x$correct, logical(1)))
  glue_env$.num_total <- length(rets)
  glue_env$.is_correct <- glue_env$.num_correct == length(rets)

  if (glue_env$.is_correct) {
    # all success found
    success(glue_with_env(glue_env, success_msg))
  } else {
    # no success_found
    failure(glue_with_env(glue_env, no_success_msg))
  }

}
