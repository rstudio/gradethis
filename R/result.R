# TODO documentation
#' @export
result <- function(x, message, correct = FALSE) {
  structure(class = "grader_result", list(
    x = x,
    message = message,
    correct = correct
  ))
}
