

#' @title Deprecated
#' @keywords internal
#' @aliases gradethis-deprecated
#' @name gradethis-deprecated
NULL


#' @describeIn gradethis-deprecated Removed from package
#' @export
grade_feedback <- function(...) {
  lifecycle::deprecate_warn("0.2.0", "grade_feedback()")

  ret <- feedback(...)
  class(ret) <- "grader_feedback"
  ret
}
