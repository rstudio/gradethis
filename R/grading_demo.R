#' Grading Demo
#'
#' If you are using the RStudio IDE, \code{grading_demo()} opens an example learnr
#' file that demonstrates how to use the grader package to check student code.
#'
#' The tutorial sets the learnr exercise. checker option to
#' \code{grade_learnr()} in the document's setup chunk.
#' It then uses three different exercise checking methods: \code{\link{check_result}}, \code{\link{test_result}}, and \code{\link{check_code}}.
#' To use a checking method, follow the exercise chunk with a chunk whose label
#' matches the label of the exercise chunk (ex: \code{myexercise}) but includes the suffix
#' \code{-check} (ex: \code{myexercise-check}). Call any checking method in that chunk. To ensure that
#' checking method can provide informative feedback, you may provide custom \code{correct} and \code{incorrect} messages.
#'
#' If you are not using RStudio IDE, you can access the demo file at \code{system.file("extdata", "grading-demo/grading-demo.Rmd", package = "grader")}.
#'
#' @export
#' @importFrom utils browseURL
grading_demo <- function() {
  grading_demo_path <- system.file("tutorials", "grading-demo/grading-demo.Rmd", package = "grader")
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(grading_demo_path)
  } else {
    browseURL(paste0("file://", grading_demo_path))
  }
}
