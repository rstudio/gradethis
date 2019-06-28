# Fail if condition matches
#' @export
fail_if <- function(x, message) {
  condi(x, message, correct = FALSE)
}

#' Pass if condition matches
#' @export
pass_if <- function(x, message) {
  condi(x, message, correct = TRUE)
}

#' Condition object
#' Captures what the user passes into \code{pass_if} or \code{fail_if},
#' figures out what type of object was passed into \code{x},
#' and returns an object that will be passed into \code{evaluate_condi}
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
#' @returns Boolean of the condition
#' @export
evaluate_condi <- function(condi, grader_args, learnr_args) {
  checkmate::assert_class(condi, "grader_condition")
  switch(condi$type,
         "formula" = evaluate_condi_formula(condi$x, grader_args$solution_quo, learnr_args$envir_prep), # nolint
         "function" = evalutate_condi_function,
         "value" = evaluate_condi_value
         )
}

evaluate_condi_formula <- function(formula, user_answer, env) {
  rlang::eval_tidy(
    formula[[2]],
    data = list(.result = user_answer, . = user_answer),
    env = env
  )
}
