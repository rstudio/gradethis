#' Grading Demo
#'
#' If you are using the RStudio IDE, `gradethis_demo()` opens an example learnr
#' file that demonstrates how to use the grader package to check student code.
#'
#' The tutorial sets the learnr `exercise.checker` option to
#' `grade_learnr()` in the document's setup chunk.
#'
#' It then uses three different exercise checking methods:
#' [grade_result()], [grade_conditions()], and [grade_code()].
#'
#' To use a checking method, follow the exercise chunk with a chunk whose label
#' matches the label of the exercise chunk (ex: `myexercise`) but includes the suffix
#' `-check` (ex: `myexercise-check`). Call any checking method in that chunk.
#'
#' To ensure that checking method can provide informative feedback,
#' you may provide custom `correct` and `incorrect` messages.
#'
#' If you are not using RStudio IDE, you can access the demo file at
#' `system.file("extdata", "grading-demo/grading-demo.Rmd", package = "grader")`.
#'
#' @export
#' @importFrom utils browseURL
gradethis_demo <- function() {
  gradethis_demo_path <- system.file("tutorials", "grading-demo/grading-demo.Rmd",
                                     package = "gradethis")
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(gradethis_demo_path)
  } else {
    browseURL(paste0("file://", gradethis_demo_path))
  }
  message("I've opened a demo tutorial for you that uses gradethis.\nIf you are using the RStudio IDE, please check your source pane,\notherwise please check your web browser.")
}
