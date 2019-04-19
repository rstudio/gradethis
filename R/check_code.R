#' Strict exercise checking
#'
#' \code{strict_check()} compares user code to a solution (i.e. model code) and
#' describes the first way that the user code differs. If the user code exactly
#' matches the solution, \code{strict_check()} returns a customizable success
#' message.
#'
#' \code{strict_check()} provides a *strict* check in that the user code must
#' exactly match the solution. It is not enough for the user code to be
#' equivalent to the solution code (e.g. to return the same result as the
#' solution).
#'
#' You can provide solution code for \code{strict_check()} to use in two ways:
#'
#' 1. Pass code as a character string or a quoted expression to the solution
#' argument of \code{strict_check()}
#'
#' 2. Make a "-solution" code chunk for the exercise to be checked in a learnr
#' document. There is no need to supply a solution argument for
#' \code{strict_check()} if you call it from the "-check" chunk of the same
#' exercise. Likewise, there is no need to supply a user argument when you call
#' \code{strict_check()} from a learnr document (learnr will provide the code
#' that the student submits when it runs \code{strict_check()}.
#'
#' For best results, name all arguments provided in the solution code.
#'
#' @param success A character string to display if the student answer matches
#'   the solution code
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
  solution = NULL, user = NULL, # provided by `grade_learnr`
  ... # ignored / extra params
) {
  chkm8_single_character(correct)
  chkm8_single_character(incorrect)

  is_same_info <- code_is_same(user, solution)

  if (is_same_info$correct) {
    return(
      result(
        x = user,
        message = glue::glue_data(
          list(
            correct = TRUE,
            message = NULL
          ),
          correct
        ),
        correct = TRUE
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
    result(
      x = user,
      message = message,
      correct = FALSE
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
    return(result(x = user, message = NULL, correct = TRUE))
  }

  message <- detect_mistakes(user, solution)
  if (is.null(message)) {
    # found no errors
    return(result(x = user, message = NULL, correct = TRUE))
  }

  return(
    result(x = user, message = message, correct = FALSE)
  )
}
