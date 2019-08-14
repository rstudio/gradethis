#' Condition object
#'
#' Captures what the student passes into \code{\link{pass_if}} or \code{\link{fail_if}},
#' figures out what type of object was passed into \code{x},
#' and returns a \code{grader_condition} object that will be passed into
#' \code{\link{evaluate_condition}}.
#'
#' @template x_condition
#' @param message character string for message returned (usually passed in from
#'    \code{\link{pass_if}} or \code{\link{fail_if}}
#' @param correct logical whether the condition is the correct answer
#'
#' @return a \code{grader_condition} object that contains
#'   the expression \code{x},
#'   the message \code{message},
#'   whether or not the expression is the correct answer or not, \code{correct},
#'   the type of expression (formula, function, or value), \code{type}
#'
#' @describeIn condition
#'    A condition object that contains the expression, message,
#'    correct status, and condition type
#' @seealso \code{\link{pass_if}}, \code{\link{fail_if}}, and \code{\link{condition}}
#' @export
#'
#' @examples
#' \dontrun{grading_demo()}
#'
#' condition(~ identical(x = .result, 5), message = "Correct", correct = TRUE)
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
