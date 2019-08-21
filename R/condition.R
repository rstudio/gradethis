#' Condition object
#'
#' Captures what the student passes into [pass_if()] or [fail_if()],
#' figures out what type of object was passed into `x`,
#' and returns a `grader_condition` object that will be passed into
#' [evaluate_condition()].
#'
#' @param x A formula, function, or value, that returns `TRUE` or `FALSE`.
#'    When comparing objects that are greater than length 1
#'    (e.g., vectors, dataframes, matricies, etc)
#'    A boolean vector will be returned if the user uses `==`, not a single boolean value.
#'    `gradethis` will run the vector through
#'     `all(..., na.rm = TRUE)` to check for the boolean value.
#'    It is advised that the user use `identical()` instead of `==` in this case.
#'
#' @param message character string for message returned (usually passed in from
#'    [pass_if()] or [fail_if()]
#'
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
#' \dontrun{gradethis_demo()}
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
