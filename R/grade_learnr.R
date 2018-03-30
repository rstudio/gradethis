#' grade_learnr
#'
#' A checker function to use with learnr
#'
#' For exercise checking, learnr tutorials require a function that learnr can
#' use in the background to run the code in each "-check" chunk and to format
#' the results into a format that learnr can display. The function must accept a
#' specific set of inputs and return a specific type of output. Users are not
#' intended to use the function themselves, but to pass it to the
#' \code{exercise.checker} knitr chunk option within the setup chunk of the
#' tutorial.
#'
#' The grader package provides \code{grade_learnr()} for this purpose. To enable
#' exercise checking in your learnr tutorial, set
#' \code{tutorial_options(exercise.checker = grade_learnr)} in the setup chunk
#' of your tutorial.
#'
#' Run \code{grading_demo()} to see an example learnr document
#' that uses \code{grade_learnr()}.
#'
#' @param label Label for exercise chunk
#' @param solution_code R code submitted by the user
#' @param user_code 	Code provided within the “-solution” chunk for the exercise.
#' @param check_code 	Code provided within the “-check” chunk for the exercise.
#' @param envir_result 	The R environment after the execution of the chunk.
#' @param evaluate_result The return value from the \code{evaluate::evaluate} function.
#' @param ... Unused (include for compatibility with parameters to be added in the future)
#'
#' @return An R list which contains several fields indicating the result of the check.
#' @export
#'
#' @examples
#' \dontrun{grading_demo()}
grade_learnr <- function(label = NULL,
                         solution_code = NULL,
                         user_code = NULL,
                         check_code = NULL,
                         envir_result = NULL,
                         evaluate_result = NULL,
                         ...) {

  # Sometimes no user code is provided, but
  # that means there is nothing to check. Also,
  # you do not want to parse NULL
  if (is.null(user_code)) {
    return(list(
      message = "I didn't receive your code. Did you write any?",
      correct = FALSE,
      type = "error",
      location = "append"
    ))
  } else {
    user_code <- parse(text = user_code)
    if (length(user_code) == 0) {
      return(list(
        message = "I didn't receive your code. Did you write any?",
        correct = FALSE,
        type = "error",
        location = "append"
      ))
    }
  }

  # Sometimes no solution is provided, but that
  # means there is nothing to check against. Also,
  # you do not want to parse NULL
  if (is.null(solution_code)) {
    return(list(
      message = "No solution is provided for this exercise.",
      correct = TRUE,
      type = "info",
      location = "append"
    ))
  } else {
    solution_code <- parse(text = solution_code)
    if (length(solution_code) == 0) {
      return(list(
        message = "No solution is provided for this exercise.",
        correct = TRUE,
        type = "info",
        location = "append"
      ))
    }
  }

  # Run checking code to get feedback
  grading_code <- pryr::standardise_call(parse(text = check_code)[[1]])
  grading_code$user <- rlang::as_quosure(user_code[[length(user_code)]])
  grading_code$solution <- rlang::as_quosure(solution_code[[length(solution_code)]])

  feedback <- eval(grading_code)

  # Check that the student submission was correct
  if (feedback == grading_code$success) {
    result <- list(
      message = feedback,
      correct = TRUE,
      type = "success",
      location = "append"
    )
  } else {
    result <- list(
      message = feedback,
      correct = FALSE,
      type = "error",
      location = "append"
    )
  }
  result
}
