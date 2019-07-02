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
  res <- tryCatch({
    switch(condi$type,
           "formula" = evaluate_condi_formula(condi$x, grader_args$solution_quo, learnr_args$envir_prep), # nolint
           "function" = evaluate_condi_function(condi$x, grader_args$solution_quo),
           "value" = evaluate_condi_value(condi$x, grader_args$solution_quo)
         )
  }, error = function(e) {
        res <- graded(correct = FALSE, message = NULL)
  })

  if (is.null(res)) return(NULL) ## when would this ever be null in this example? res is either T/F or graded obj
  if (is.logical(res)) {
    if (res) {
      # condi$x returned TRUE, meaning a match was found
      return(graded(correct = condi$correct, message = condi$message))
    } else {
      # if a match was not found (condi$x is FALSE)
      return(NULL)
    }
  }

  if (! inherits(res, 'grader_graded')) {
    stop(glue::glue(
      "I expected a grader_graded object, a logical, or a NULL value. ",
      "Received {paste0(class(res), collapse = ', ')}"))
  }
  return(res) # return the error from tryCatch
}

evaluate_condi_formula <- function(formula, user_answer, env) {
  rlang::eval_tidy(
    formula[[2]],
    data = list(.result = user_answer, . = user_answer),
    env = env
  )
}

evaluate_condi_function <- function(fxn, user_answer) {
  fxn_results <- fxn(user_answer)
  checkmate::expect_logical(fxn_results, len = 1)
  return(fxn_results)
}

evaluate_condi_value <- function(val, user_answer) {
  identical(val, user_answer)
}
