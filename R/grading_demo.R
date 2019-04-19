#' Grading Demo
#'
#' If you are using the RStudio IDE, \code{grading_demo()} opens an example learnr
#' file that demonstrates how to use the grader package to check student code.
#'
#' The tutorial sets the learnr exercise. checker option to
#' \code{grade_learnr()} in the document's setup chunk. It then uses
#' \code{strict_check()} to check the single exercise in the tutorial. To use
#' \code{strict_check()}, follow the exercise chunk with a chunk whose label
#' matches the label of the exercise chunk but includes the suffix
#' \code{-check}. Call \code{strict_check()} in that chunk. To ensure that
#' \code{strict_check()} can provide formative feedback, also provide the
#' solution code in a chunk suffixed \code{-solution}.
#'
#' If you are not using RStudio IDE, you can access the demo file at \code{system.file("extdata", "grading-demo/grading-demo.Rmd", package = "grader")}.
#'
#' @export
grading_demo <- function() {
  grading_demo_path <- system.file("tutorials", "grading-demo/grading-demo.Rmd", package = "grader")
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(grading_demo_path)
  } else {
    browseURL(paste0('file://', grading_demo_path))
  }
}
