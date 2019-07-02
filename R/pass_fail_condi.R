#' Pass if condition matches
#' @export
pass_if <- function(x, message) {
  condi(x, message, correct = TRUE)
}

# Fail if condition matches
#' @export
fail_if <- function(x, message) {
  condi(x, message, correct = FALSE)
}

#' Condition object
#' TODO rename to condition
#' Captures what the user passes into \code{pass_if} or \code{fail_if},
#' figures out what type of object was passed into \code{x},
#' and returns an object that will be passed into \code{evaluate_condi}
#' @export
condi <- function(x, message, correct) {
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
#' @returns a \code{graded} value or \code{NULL} if the condition is \code{FALSE}
#' @export
evaluate_condi <- function(condi, grader_args, learnr_args) {
  checkmate::assert_class(condi, "grader_condition")
  res <- switch(condi$type,
           "formula" = evaluate_condi_formula(condi$x, grader_args$solution_quo, learnr_args$envir_prep), # nolint
           "function" = evaluate_condi_function(condi$x, grader_args$solution_quo),
           "value" = evaluate_condi_value(condi$x, grader_args$solution_quo)
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
