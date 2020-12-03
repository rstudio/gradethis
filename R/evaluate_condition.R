#' Evaluates a condition
#'
#' \lifecycle{superseded} Please use [grade_this()] mixed with [pass()], [pass_if_equal()], [fail()], and/or [fail_if_equal()]. Can also use [eval_gradethis()].
#'
#' Evaluates the [condition()] object to return a [graded()] value.
#'
#' @param condition a [condition()] object
#' @param ... ignored
#' @inheritParams grade_learnr
#' @param env environment to evaluate the condition
#'
#' @return a [graded()] value if `condi$x` is `TRUE` or
#'   `NULL` if `condi$x` is `FALSE`
#' @export
#'
#' @examples
#' condi_formula_t <- condition(
#'   ~ identical(.result, 5),
#'   message = "my correct message",
#'   correct = TRUE
#' )
#' evaluate_condition(
#'   condi_formula_t,
#'   last_value = 5,
#'   env = new.env()
#' )
evaluate_condition <- function(condition, ..., last_value, env) {
  checkmate::assert_class(condition, "gradethis_condition")
  ellipsis::check_dots_empty()

  err_msg <- NULL
  res <- tryCatch(
    {
      switch(
        condition$type,
        "formula" = evaluate_condi_formula(
          condition$x,
          last_value,
          env = env
        ),
        "function" = evaluate_condi_function(condition$x, last_value),
        "value" = evaluate_condi_value(condition$x, last_value)
      )
    },
    error = function(e) {
      err_msg <<- conditionMessage(e)
    }
  )

  if (!is.null(err_msg)) {
    return(legacy_graded(correct = FALSE, message = err_msg))
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
  # if (inherits(res, 'gradethis_graded')) {return(res)} # nolint
  if (is.null(res)) return(NULL)

  checkmate::assert_logical(res, len = 1, null.ok = FALSE)
  if (res) {
    legacy_graded(correct = condition$correct, message = condition$message)
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
