#' Grading Demo
#'
#' If you are using the RStudio IDE, `grading_demo()` opens an example learnr
#' file that demonstrates how to use the grader package to check student code.
#' 
#' The tutorial sets the learnr checker option to `grade_learnr()` in the
#' document's setup chunk. It then uses `strict_check()` to check the single
#' exercise in the tutorial. To use `strict_check()`, follow the exercise chunk
#' with a chunk whose label matches the label of the exercise chunk but includes
#' the suffix `-check`. Call `strict_check()` in that chunk. To ensure that
#' `strict_check()` can provide formative feedback, also provide the solution
#' code in a chunk suffixed `-solution`.
#' 
#' If you are not using RStudio IDE, you can access the demo file at `system.file("extdata", "grading-demo/grading-demo.Rmd", package = "grader")`.
#'
#' @return
#' @export
#'
#' @examples
grading_demo <- function() {
  rstudioapi::navigateToFile(system.file("extdata", "grading-demo/grading-demo.Rmd", package = "grader"))
}