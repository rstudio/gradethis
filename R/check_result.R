
#' Check result of exercise code
#'
#' \code{check_result()} compares the final result of the user code to known \code{results}.
#' If the user result exactly matches a known \code{result}, \code{check_result}
#' returns the matching message value.
#'
#' @param results A \code{\link{results}} object that contains possible \code{\link{result}} values to compare against.
#' @param correct A character string to display if the student answer matches
#'   a known answer.
#'   This character string will be run through \code{glue::\link[glue]{glue_data}} with \code{list(correct = TRUE, message = "<result message>")}. where message is the matched result message.
#' @param incorrect A character string to display if the student answer matches
#'   a known answer.
#'   This character string will be run through \code{glue::\link[glue]{glue_data}} with \code{list(correct = FALSE, message = "<result message>")}. where message is the matched result message.
#' @param empty_msg A character string to display as a message if the user code is NULL.
#' @param ... ignored
#' @param user (Optional) student code to check against the \code{results} surrounded
#'   by \code{quote()}, \code{rlang::quo()}, or provided as a character string.
#'
#' @return a \code{grader_result} structure from \code{\link{result}} containing a formatted \code{correct} or \code{incorrect} message.
#'
#' @export
#' @examples
#' \dontrun{grading_demo()}
check_result <- function(
  results,
  correct = paste0(
    "{random_praise()}",
    "{paste0(if (!is.null(message)) paste0(\" \", message))}"
  ),
  incorrect = paste0(
    "{paste0(if(!is.null(message)) paste0(message, \" \"))}",
    "{random_encourage()}"
  ),
  empty_msg = "I did not notice a result. Does your code return one?",
  ..., # ignored / extra params
  user = NULL # provided by `grade_learnr`
) {
  chkm8_class(results, "grader_results")
  chkm8_single_character(correct)
  chkm8_single_character(incorrect)

  if (!any(vapply(results, `[[`, logical(1), "correct"))) {
    stop("At least one correct result must be provided")
  }

  user_answer <- get_user_code(user)
  if (is.null(user_answer)) {
    return(result(user_answer, message = empty_msg, correct = FALSE))
  }

  for (resu in results) {
    if (identical(resu$x, user_answer)) {
      message <- glue::glue_data(
        list(
          correct = resu$correct,
          message = resu$message
        ),
        {if (resu$correct) correct else incorrect}
      )

      # print("user_answer == ans$x")
      return(result(
        x = resu$x,
        message = message,
        correct = resu$correct
      ))
    }
  }
  # print("not found")
  return(result(
    x = structure("answer not found", class = c("exercise_answer_not_found", "character")),
    message = glue::glue_data(
      list(
        message = NULL
      ),
      incorrect
    ),
    correct = FALSE
  ))
}


#' Possible results
#'
#' A list of possible results to check against.  Each argument supplied must contain output from \code{\link{result}}.
#' @param ... a group of \code{\link{result}} output objects.
#' @export
#' @return A \code{list} with class \code{"grader_result"} containing \code{\link{result}} objects.
#' @seealso \code{\link{result}}
#' @examples
#' results(
#'   result(1, "Custom message for value 1."),
#'   result(2, "Custom message for value 2.", correct = TRUE),
#'   result(3, "Custom message for value 3."),
#'   result(4, "Custom message for value 4.", correct = TRUE)
#' )
results <- function(...) {
  x = list(...)
  lapply(x, chkm8_class, "grader_result")
  structure(
    class = "grader_results",
    x
  )
}
