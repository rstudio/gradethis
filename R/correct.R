# TODO determine if API should use `graded(correct, message)` or `correct(message)`/`incorrect(message)`
# TODO document
#' Correct or Incorrect result
#' @export
#' @rdname correct
correct <- function(message = NULL) {
  structure(
    list(message = message),
    class = c("grader_correct", "grader_rrect")
  )
}
#' @export
#' @rdname correct
incorrect <- function(message = NULL) {
  structure(
    list(message = message),
    class = c("grader_incorrect", "grader_rrect")
  )
}


#' @export
#' @rdname correct
is_rrect <- function(x) {
  inherits(x, "grader_rrect")
}


correct_fn <- correct
incorrect_fn <- incorrect
