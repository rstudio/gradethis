#' Check code structure
#'
#' \code{check_code()} compares user code to a solution (i.e. model code) and
#' describes the first way that the user code differs. If the user code exactly
#' matches the solution, \code{check_code()} returns a customizable success
#' message.
#'
#' \code{check_code()} provides a *strict* check in that the user code must
#' exactly match the solution. It is not enough for the user code to be
#' equivalent to the solution code (e.g. to return the same result as the
#' solution).
#'
#' You can provide solution code for \code{check_code()} to use in two ways:
#'
#' 1. Pass code as a character string or a quoted expression to the solution
#' argument of \code{check_code()}
#'
#' 2. Make a "-solution" code chunk for the exercise to be checked in a learnr
#' document. There is no need to supply a solution argument for
#' \code{check_code()} if you call it from the "-check" chunk of the same
#' exercise. Likewise, there is no need to supply a user argument when you call
#' \code{check_code()} from a learnr document (learnr will provide the code
#' that the student submits when it runs \code{check_code()}.
#'
#' For best results, name all arguments provided in the solution code.
#'
#' @template correct
#' @template incorrect
#' @template grader_args
#' @template learnr_args
#' @template glue_correct
#' @template glue_incorrect
#' @param glue_pipe A glue string that returns the final message displayed when a user uses a pipe,
#'    \code{$>$}. Defaults to \code{getOption("gradethis_glue_pipe")}.
#'
#' @return a \code{\link{graded}} object.
#'   An incorrect message will describe the first way that the answer differs,
#'   the message will be the content of the \code{glue_pipe} argument.
#'
#' @export
#' @examples
#' \dontrun{grading_demo()}
check_code <- function(
  correct = NULL,
  incorrect = NULL,
  grader_args = list(),
  learnr_args = list(),
  glue_correct = getOption("gradethis_glue_correct"),
  glue_incorrect = getOption("gradethis_glue_incorrect"),
  glue_pipe = getOption("gradethis_glue_pipe")
) {
  chkm8_single_character(correct)
  chkm8_single_character(incorrect)

  user <- grader_args$user_quo
  solution <- grader_args$solution_quo

  is_same_info <- code_is_same(user, solution)

  if (is_same_info$correct) {
    return(
      graded(
        correct = TRUE,
        message = glue_message(
          glue_correct,
          .is_correct = TRUE,
          .message = NULL,
          .correct = correct
        )
      )
    )
  }

  message <- glue_message(
    glue_incorrect,
    .is_correct = FALSE,
    .message = is_same_info$message,
    .incorrect = incorrect
  )
  if (uses_pipe(user)) {
    message <- glue_message(
      glue_pipe,
      .user = user,
      .message = message,
      .incorrect = incorrect
    )
  }

  return(
    graded(
      correct = FALSE,
      message = message
    )
  )
}



code_is_same <- function(user = NULL, solution = NULL) {

  # Sometimes no solution is provided, but that
  # means there is nothing to check against
  if (is.null(solution)) {
    stop("No solution is provided for this exercise.")
  }

    # Sometimes no user code is provided,
  # that means there is nothing to check
  if (is.null(user)) {
    stop("I didn't receive your code. Did you write any?")
  }

  # MUST call user first to avoid "poisoning" the envir with solution information
  user_code <- rlang::get_expr(user)
  solution_code <- rlang::get_expr(solution)

  # Correct answers are all alike
  if (identical(user_code, solution_code)) {
    return(graded(correct = TRUE, message = NULL))
  }

  message <- detect_mistakes(user, solution)
  if (is.null(message)) {
    # found no errors
    return(graded(correct = TRUE, message = NULL))
  }

  return(
    graded(correct = FALSE, message = message)
  )
}
