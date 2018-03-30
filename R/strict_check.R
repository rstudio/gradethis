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

    # Sometimes no user code is provided, but
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
                            solution,
                            .name = NULL) {

  # Stop and notify the student if their value has no
  # match in the solution (or vice versa)
  if (is.null(user) && !is.null(solution)) {
    return(expected(solution, .name))
  } else if (is.null(solution) && !is.null(user)) {
    return(did_not_expect(user, .name))

    # directly compare values that are atomics or names
  } else if (is.atomic(user) || is.name(user)) {
    if (user != solution) return(does_not_match(user, solution, .name))

    # iterate over the elements of a call (or pairlist?)
  } else {

    # calls should be treated the same
    # whether or not they use the pipe
    user <- unpipe(user)
    solution <- unpipe(solution)

    # ensure the submission and the solution use the same call
    if (user[[1]] != solution[[1]]) return(does_not_match(user, solution, .name))

    # match unnamed arguments to names as R would, then compare the named
    # elements in the submission to the named elements in the solution one
    # at a time
    user <- pryr::standardise_call(user)
    solution <- pryr::standardise_call(solution)
    named_args <- union(names(user), names(solution))
    named_args <- named_args[named_args != ""]
    first_name <- named_args[1] # it would be distracting to name the
    # first argument in feedback (e.g. .x)
    for (name in named_args) {
      if (name == first_name) {
        message <- detect_mistakes(user[[name]], solution[[name]])
      } else {
        message <- detect_mistakes(user[[name]], solution[[name]], name)
      }
      if (!is.null(message)) return(message)
    }

    # Some arguments in the submission and solution may still be unnamed. These
    # arguments were not named by the author, nor matched to a name by R. Get
    # these arguments and then compare them to each other one at a time by the
    # order that they appear in.
    if (is.null(names(user))) {
      user_unnamed <- user
    } else {
      user_unnamed <- user[names(user) == ""][-1]
    }
    if (is.null(names(solution))) {
      solution_unnamed <- solution
    } else {
      solution_unnamed <- solution[names(solution) == ""][-1]
    }

    max_length <- max(length(user_unnamed), length(solution_unnamed))

    for (i in seq_len(max_length)) {
      message <- detect_mistakes(user_unnamed[[i]], solution_unnamed[[i]])
      if (!is.null(message)) return(message)
    }
  }
}

expected <- function(this, .name = NULL) {
  if (is.null(.name)) {
    return(glue::glue(
      "I expected your code to include {deparse(this)}. You may have ",
      "referred to it in a different way, or left out an important argument name. ",
      "Please try again."
    ))
  } else {
    return(glue::glue(
      "I expected your code to include {.name} = {deparse(this)}. ",
      "You may have referred to it in a different way, or left out an important ",
      "argument name. Please try again."
    ))
  }
}

did_not_expect <- function(that, .name = NULL) {
  if (is.null(.name)) {
    return(glue::glue(
      "I did not expect your code to include {deparse(that)}. ",
      "You may have included an unnecessary value, or you may have left ",
      "out an important argument name. Please try again."
    ))
  } else {
    return(glue::glue(
      "I did not expect your code to include {.name} = {deparse(that)}. ",
      "You may have included an unnecessary value, or you may have used the wrong ",
      "argument name. Please try again."
    ))
  }
}

does_not_match <- function(user, solution, .name = NULL) {
  if (length(solution) > 1) solution <- solution[[1]]
  if (length(user) > 1) user <- user[[1]]

  if (is.null(.name)) {
    return(glue::glue(
      "I expected {deparse(solution)} where you wrote {deparse(user)}. ",
      "Please try again."
    ))
  } else {
    return(glue::glue(
      "I expected {.name} = {deparse(solution)} where you wrote ",
      "{.name} = {deparse(user)}. Please try again."
    ))
  }
}
