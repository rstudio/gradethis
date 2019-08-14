#' Get Code
#'
#' Helper methods around \code{rlang::\link[rlang]{eval_tidy}}
#' to extract user code and solution code.
#'
#' @seealso \code{\link{check_result}}, \code{\link{test_result}}, and \code{\link{check_code}}
#' @param user,solution,expr An expression or quosure to evaluate.
#' @param name Name to print if a \code{NULL} expression is provided.
#'
#' @describeIn get_code Get student code
#' @inheritParams rlang::eval_tidy
#'
#' @noRd
get_user_code <- function(user = NULL, env = rlang::caller_env()) {
  get_code(user, "user", env = env)
}

#' @describeIn get_code Get solution code provided by instructor
#""
#' @noRd
get_solution_code <- function(solution = NULL, env = rlang::caller_env()) {
  get_code(solution, "solution", env = env)
}

#' @describeIn get_code Generic get code function
#'
#' @noRd
get_code <- function(expr = NULL, name = "<name not provided>", env = rlang::caller_env()) {
  if (is.null(expr)) {
    stop("'", name, "' not provided")
  }
  rlang::eval_tidy(expr, env = env)
}
