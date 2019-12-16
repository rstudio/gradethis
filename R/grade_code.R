#' Grade code against a solution
#'
#' Checks the code expression or the code result against a solution.
#'
#' `grade_code()` compares student code to a solution (i.e. model code) and
#' describes the first way that the student code differs. If the student code
#' exactly matches the solution, `grade_code()` returns a customizable success
#' message (`correct`). If the student code does not match the solution, a
#' customizable incorrect message (`incorrect`) can also be provided.
#'
#' `grade_code()` provides a *strict* check in that the student code must
#' exactly match the solution. It is not enough for the student code to be
#' equivalent to the solution code (e.g. to return the same result as the
#' solution).
#'
#' You can provide solution code for `grade_code()` to use in two ways:
#'
#' 1. Pass code as a character string or a quoted expression to the solution
#' argument of `grade_code()`
#'
#' 2. Make a "-solution" code chunk for the exercise to be checked in a learnr
#' document. There is no need to supply a solution argument for `grade_code()`
#' if you call it from the "-check" chunk of the same exercise. Likewise, there
#' is no need to supply a student submitted code argument when you call
#' `grade_code()` from a learnr document (learnr will provide the code that the
#' student submits when it runs `grade_code()`.
#'
#' For best results, name all arguments provided in the solution code.
#'
#' @param correct A character string to display if the student answer matches a
#'   known correct answer.
#'
#' @param incorrect A character string to display if the student answer matches
#'   a known incorrect answer.
#'
#' @param grader_args A list of parameters passed to `grader` functions
#'   (provided by [grade_learnr()]). This contains:
#'
#'   * `user_quo`: Quoted R code submitted by the user. For example
#'   \code{\link[rlang:quo]{rlang::quo(1)}}
#'
#'   * `solution_quo`: (Optional) Quoted solution R code provided by the
#'   `*-solution` chunk for an exercise.
#'
#' @param learnr_args A list of all parameters passed to [grade_learnr()] by
#'   `learnr`. See
#'   <https://rstudio.github.io/learnr/exercises.html#exercise_checking> for
#'   more details.
#'
#' @param glue_correct A glue string that returns the final correct message
#'   displayed. Defaults to `getOption("gradethis_glue_correct")`.
#'
#' @param glue_incorrect A glue string that returns the final incorrect message
#'   displayed. Defaults to `getOption("gradethis_glue_incorrect")`.
#'
#' @param glue_pipe A glue string that returns the final message displayed when
#'   the student uses a pipe, `$>$`. Defaults to
#'   `getOption("gradethis_glue_pipe")`.
#'
#' @return a [graded()] object. An incorrect message will describe the first way
#'   that the answer differs, the message will be the content of the `glue_pipe`
#'   argument.
#'
#' @seealso [grade_code()], [grade_result()], and [grade_conditions()]
#' @export
#' @examples
#' \dontrun{gradethis_demo()}
#'
#' # This is a manual example, see grading demo for `learnr` tutorial usage
#' y <- quote(sqrt(log(2)))
#' z <- quote(sqrt(log(1)))
#' grade_code(grader_args = list(user_quo = y, solution_quo = z))
grade_code <- function(
  correct = NULL,
  incorrect = NULL,
  grader_args = list(),
  learnr_args = list(),
  glue_correct = getOption("gradethis_glue_correct"),
  glue_incorrect = getOption("gradethis_glue_incorrect"),
  glue_pipe = getOption("gradethis_glue_pipe")
) {
  user <- grader_args$user_quo
  solution <- grader_args$solution_quo

  # MUST call user first to avoid "poisoning" the envir with solution information
  user <- rlang::get_expr(user)
  solution <- rlang::get_expr(solution)

  if (is_code_identical(user, solution)) {
    is_same_info <- graded(correct = TRUE, message = NULL)
  } else {
    # if (as.character(user[[1]]) == "test_fn") {browser()}
    message <- detect_mistakes(user, solution)
    if (is.null(message)) {
      # found no errors
      is_same_info <- graded(correct = TRUE, message = NULL)
    } else {
      is_same_info <- graded(correct = FALSE, message = message)
    }
  }

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



is_code_identical <- function(user = NULL, solution = NULL) {

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

  # Correct answers are all alike
  if (identical(user, solution)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
