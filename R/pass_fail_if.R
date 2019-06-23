# example: pass_if(~ .result == 5, message = "passing message")
# need to capture ~ .result == 5 as a quosure
# 
pass_if_formula <- function(formula, user_answer, grader_args, learnr_args) {
  form_rhs_quo <- rlang::as_quosure(formula[[2]], env = learnr_args$envir_prep)
  return(rlang::eval_tidy(form_rhs_quo,
                          data = list(
                            .result = user_answer,
                            . = user_answer
                          )))
}

pass_if <- function(x, message, user_answer, grader_args = list(), learnr_args = list()) {
  if (rlang::is_formula(x)) {
    match <- pass_if_formula(x, user_answer, grader_args, learnr_args)
  }
  return(match)
}
