#' Evaluates a condition
#'
#' Evaluates the [condition()] object to return a [graded()] value.
#'
#' @param condition a [condition()] object
#' @param grader_args at minimum, a list that just contains the value for `solution_quo`
#' @param learnr_args at minimum, a list that just contains the value for `envir_prep`
#'
#' @return a [graded()] value if `condi$x` is `TRUE` or
#'   `NULL` if `condi$x` is `FALSE`
#' @export
#'
#' @examples
#'  condi_formula_t <- condition(~ identical(.result, 5),
#'                               message = "my correct message",
#'                               correct = TRUE)
#'  grader_args <- list()
#'  learnr_args <- list(last_value = quote(5), envir_prep = new.env())
#'  evaluate_condition(condi_formula_t, grader_args, learnr_args)
evaluate_condition <- function(condition, grader_args, learnr_args) {
  checkmate::assert_class(condition, "grader_condition")

  err_msg <- NULL
  res <- tryCatch({
    switch(condition$type,
           "formula" = evaluate_condi_formula(condition$x, learnr_args$last_value, env = learnr_env(learnr_args)), # nolint
           "function" = evaluate_condi_function(condition$x, learnr_args$last_value),
           "value" = evaluate_condi_value(condition$x, learnr_args$last_value)
         )
  }, error = function(e) { # nolint
    err_msg <<- e$message
  })

  if (!is.null(err_msg)) {
    return(graded(correct = FALSE, message = err_msg))
  }

  # if we compare something like a vector or dataframes to one another
  # we need to collapse the result down to a single boolean value
  if (length(res) > 1) {
    ## this isn't the best way to handle NA values so we raise a warning.
    ## https://github.com/rstudio-education/grader/issues/46 # nolint
    warning(glue::glue("I got a length of {length(res)}, instead of 1 during the conditional check.\n Did you use == ? If so, consider using identical()")) # nolint
    res <- !all(is.na(res)) && all(res, na.rm = TRUE)
  }

  # implement when we add a `exec`/`expect` api to grade_result
  # will account for function returns
  # if (inherits(res, 'grader_graded')) {return(res)} # nolint
  if (is.null(res)) return(NULL)

  checkmate::assert_logical(res, len = 1, null.ok = FALSE)
  if (res) {
    graded(correct = condition$correct, message = condition$message)
  } else {
    NULL
  }
}

evaluate_condi_formula <- function(formula, user_answer, env) {
  rlang::eval_tidy(
    formula[[2]],
    data = list(.result = user_answer, . = user_answer),
    env = env
  )
}

evaluate_condi_function <- function(fxn, user_answer) {
  fxn(user_answer)
}

evaluate_condi_value <- function(val, user_answer) {
  identical(val, user_answer)
}
