#' Graded object for submission value
#'
#'The return value from `graded` should be returned by every
#'`*-check` chunk when used with \code{link{grade_learnr}}.
#'
#' @param message A character string of the message to be displayed.
#' @param correct A boolean value of whether or not the checked code is correct.
#' @export
graded <- function(correct, message = NULL) {
  chkm8_single_character(message)
  checkmate::expect_logical(correct, any.missing = FALSE, len = 1, null.ok = FALSE)

  ret <- list(
    message = message %||% "",
    correct = correct
  )
  class(ret) <- "grader_graded"
  ret
}
