#' Condition object
#'
#' Captures what the student passes into [pass_if()] or [fail_if()],
#' figures out what type of object was passed into `x`,
#' and returns a `grader_condition` object that will be passed into
#' [evaluate_condition()].
#'
#' @template x_condition
#' @param message character string for message returned (usually passed in from
#'    [pass_if()] or [fail_if()]
#' @param correct logical whether the condition is the correct answer
#'
#' @return a `grader_condition` object that contains
#'   the expression `x`,
#'   the message `message`,
#'   whether or not the expression is the correct answer or not, `correct`,
#'   the type of expression (formula, function, or value), `type`
#'
#' @describeIn condition
#'    A condition object that contains the expression, message,
#'    correct status, and condition type
#' @seealso [pass_if()], [fail_if()], and [condition()]
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
