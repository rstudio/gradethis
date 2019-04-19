# TODO documentation
#' @export
result <- function(x, message, correct = FALSE) {
  structure(class = "grader_result", list(
    x = x,
    message = message,
    correct = correct
  ))
}

# TODO documentation
#' @export
results <- function(...) {
  x = list(...)
  lapply(x, chkm8_class, "grader_result")
  structure(
    class = "grader_results",
    x
  )
}
