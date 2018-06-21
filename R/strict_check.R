#' strict_check
#'
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
#' @return (character) A message. If the student answer differs from the
#'   solution code, the message will describe the first way that the answer
#'   differs, and it will ask the student to try again. If the answer matches
#'   the solution code, the message will be the content of the \code{success}
#'   argument.
#'
#' @export
#'
#' @examples
#' \dontrun{grading_demo()}
strict_check <- function(success = "Correct!",
                         solution = NULL,
                         user = NULL) {

  # Sometimes no solution is provided, but that
  # means there is nothing to check against
  if (is.null(solution)) {
    stop("No solution is provided for this exercise.")

    # Sometimes no user code is provided,
    # that means there is nothing to check
  } else if (is.null(user)) {
    stop("I didn't receive your code. Did you write any?")

    # Correct answers are all alike
  } else if (suppressWarnings(user == solution)) {
    return(success)

    # But incorrect answers are each incorrect in their own way
    # (and we should let the student know how their answer is
    # incorrect)
  } else {
    message <- detect_mistakes(user, solution)
    if (is.null(message)) {
      return(success)
    } else {
      return(message)
    }
  }
}

detect_mistakes <- function(user,
                            solution) {
  
  # code should be checked in the order 
  # of evaluation, whether or not the 
  # student (and/or teacher) used a pipe
  user <- order_calls(unpipe_all(user))
  solution <- order_calls(unpipe_all(solution))
  
  max_length <- max(length(user), length(solution))
  
  for (i in seq_len(max_length)) {
    if (i > length(user)) return("you missed something!")
    if (i > length(solution)) return("you wrote too much!")
    
    if (user[[i]] != solution[[i]]) {
      return(does_not_match(user[[i]], solution[[i]]))
    }
  }
  NULL
}