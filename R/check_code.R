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
#' @param correct A character string to display if the student answer matches
#'   the solution code.
#'   This character string will be run through \code{glue::\link[glue]{glue_data}} with \code{list(correct = TRUE, message = NULL)}.
#' @param incorrect A character string to display if the student answer matches
#'   the solution code.
#'   This character string will be run through \code{glue::\link[glue]{glue_data}} with \code{list(correct = FALSE, message = "<STRING>")} where message is the error found while comparing the user solution to the known solution.
#' @param solution (Optional) solution code surrounded by \code{quote()},
#'   \code{rlang::quo()}, or provided as a character string.
#' @param user (Optional) student code to check against the solution surrounded
#'   by \code{quote()}, \code{rlang::quo()}, or provided as a character string.
#'
#' @return a \code{grader_result} structure from \code{\link{result}}.
#'   If the student answer differs from the
#'   solution code, the message will describe the first way that the answer
#'   differs, and it will ask the student to try again. If the answer matches
#'   the solution code, the message will be the content of the \code{success}
#'   argument.
#'
#' @export
#' @examples
#' \dontrun{grading_demo()}
check_code <- function(
  correct = "{random_praise()} Correct!",
  incorrect = "{message} {random_encourage()}",
  grader_args = list(), # provided by `grade_learnr`
  learnr_args = list() # provided by `grade_learnr`
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
        message = glue::glue_data(
          list(
            correct = TRUE,
            message = NULL
          ),
          correct
        )
      )
    )
  }

  message <- glue::glue_data(
    list(
      correct = FALSE,
      message = is_same_info$message
    ),
    incorrect
  )
  if (uses_pipe(user)) {
    message <- glue::glue_data(
      list(
        user = user,
        message = message
      ),
      "I see that you are using pipe operators (e.g. %>%), ",
      "so I want to let you know that this is how I am interpretting your code ",
      "before I check it:\n\n{deparse(unpipe_all(user))}\n\n{message}"
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
  if (suppressWarnings(user_code == solution_code)) {
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
