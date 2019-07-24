#' Pass if condition matches
#' @param x a formula, function, or value, that returns \code{TRUE} or \code{FALSE}
#' @param message chracter string for message returned
#' @export
pass_if <- function(x, message = NULL) {
  condition(x, message, correct = TRUE)
}

#' Fail if condition matches
#' @param x a formula, function, or value, that returns \code{TRUE} or \code{FALSE}
#' @param message chracter string for message returned
#' @export
fail_if <- function(x, message = NULL) {
  condition(x, message, correct = FALSE)
}

#' Condition object
#' Captures what the user passes into \code{\link{pass_if}} or \code{\link{fail_if}},
#' figures out what type of object was passed into \code{x},
#' and returns a \code{grader_condition} object that will be passed into \code{evaluate_condi}
#'
#' @param x expression to be evaluated
#' @param message character string for message returned
#' @param correct logical whether the condition is the correct answer
#'
#' @return a \code{grader_condition} object that contains
#'   the expression \code{x},
#'   the message \code{message},
#'   whether or not the expression is the correct answer or not, \code{correct},
#'   the type of expression (formula, function, or value), \code{type}
#' @export
condition <- function(x, message, correct) {
  type <-
    if (rlang::is_formula(x)) {
      "formula"
    } else if (rlang::is_function(x)) {
      "function"
    } else {
      "value"
    }

  ret <- list(
    x = x,
    message = message,
    correct = correct,
    type = type
  )
  class(ret) <- "grader_condition"
  ret
}

#' Evaluates a condition
#'
#' @param condi a \code{grader} \code{\link{condition}} object
#' @param grader_args at minimum, a list that just contains the value for \code{solution_quo}
#' @param learnr_args at minimum, a list that just contains the value for \code{envir_prep}
#'
#' @return a \code{graded} value if \code{condi$x} is \code{TRUE} or
#'   \code{NULL} if \code{condi$x} is \code{FALSE}
#' @export
evaluate_condition <- function(condi, grader_args, learnr_args) {
  checkmate::assert_class(condi, "grader_condition")

  res <- switch(condi$type,
           "formula" = evaluate_condi_formula(condi$x, learnr_args$last_value, learnr_args$envir_prep), # nolint
           "function" = evaluate_condi_function(condi$x, learnr_args$last_value),
           "value" = evaluate_condi_value(condi$x, learnr_args$last_value)
         )

  # implement when we add a `exec`/`expect` api to check_result
  # will account for function returns
  # if (inherits(res, 'grader_graded')) {return(res)} # nolint
  if (is.null(res)) return(NULL)

  checkmate::assert_logical(res, len = 1, null.ok = FALSE)
  if (res) {
    return(graded(correct = condi$correct, message = condi$message))
  } else {
    return(NULL)
  }
}

evaluate_condi_formula <- function(formula, user_answer, env) {
  form_result <- rlang::eval_tidy(
    formula[[2]],
    data = list(.result = user_answer, . = user_answer),
    env = env
  )
  return(form_result)
}

evaluate_condi_function <- function(fxn, user_answer) {
  fxn_result <- fxn(user_answer)
  return(fxn_result)
}

evaluate_condi_value <- function(val, user_answer) {
  val_result <- identical(val, user_answer)
  return(val_result)
}
