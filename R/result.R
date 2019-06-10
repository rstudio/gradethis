#' Result
#'
#' The \code{result} method returns an object containing information about what has been graded or what could be graded against.
#' @param x object graded or being compared against
#' @param message possible message value to be displayed
#' @param correct a boolean that determines if the result is a correct result
#' @export
#' @examples
#' result(1, "Custom message for value 1.")
#' result(2, "Custom message for value 2.", correct = TRUE)
#' result(3, "Custom message for value 3.")
#' result(4, "Custom message for value 4.", correct = TRUE)
#' \dontrun{grading_demo()}
result <- function(x, message = NULL, correct = FALSE) {
  chkm8_single_character(message)
  checkmate::expect_logical(correct, any.missing = FALSE, len = 1, null.ok = FALSE)

  structure(class = "grader_result", list(
    x = x,
    message = message %||% "",
    correct = correct
  ))
}


#' Graded submission value
#'
#'The return value from \code{graded} should be returned by every \code{*-check} chunk when used with \code{link{grade_learnr}}.
#'
#' @param message A character string of the message to be displayed.
#' @param correct A boolean value of whether or not the checked code is correct.
#' @export
graded <- function(correct, message = NULL) {
  chkm8_single_character(message)
  checkmate::expect_logical(correct, any.missing = FALSE, len = 1, null.ok = FALSE)

  structure(class = "grader_graded",
    list(
      message = message %||% "",
      correct = correct
    )
  )
}
