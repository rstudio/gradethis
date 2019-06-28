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

evaluate_condi <- function(condi_x, grader_args, learnr_args) {
  checkmate::assert_class(condi_x, "grader_condition")
  switch(condi_x$type,
         "formula" = evaluate_condi_formula(condi_x, grader_args$solution_quo, learnr_args$envir_prep),
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



# example: pass_if(~ .result == 5, message = "passing message")
# need to capture ~ .result == 5 as a quosure
# # 
# pass_if_formula <- function(formula, user_answer, grader_args, learnr_args) {
#   form_rhs_quo <- rlang::as_quosure(formula[[2]], env = learnr_args$envir_prep)
#   return(rlang::eval_tidy(form_rhs_quo,
#                           data = list(
#                             .result = user_answer,
#                             . = user_answer
#                           )))
# }



# snippet code for a function
# tryCatch( {
#   ret <- fn(.last_value)
#   if (!inherits(ret, "grader_graded")) {
#     if (ret) {
#       graded(TRUE, correct)
#     } else {
#       graded(FALSE< incorrect)
#     }
#   } else {
#     ret
#   }
# }, error = function(e) {
#   message <- ... error = as.character(e)
#   graded(FALSE, message)
# })


# pass_if(checkmate::assert_numeric)