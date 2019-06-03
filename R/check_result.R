
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
  ...,
  correct = "{paste0(random_praise(), if (nchar(message) > 0) \" \", message)}",
  incorrect = "{paste0(message, if(nchar(message) > 0) \" \", random_encourage())}",
  empty_msg = "I did not notice a result. Does your code return one?",
  grader_args = list(), # provided by `grade_learnr`
  learnr_args = list() # provided by `grade_learnr`
) {
  results <- list(...)
  chkm8_item_class(results, "grader_result")
  chkm8_single_character(correct)
  chkm8_single_character(incorrect)
  chkm8_single_character(empty_msg)

  if (!any(vapply(results, `[[`, logical(1), "correct"))) {
    stop("At least one correct result must be provided")
  }

  user_answer <- learnr_args$last_value

  # init final answer as not found
  final_result <- graded(correct = FALSE, "Answer not found")
  for (resu in results) {
    if (identical(resu$x, user_answer)) {
      final_result <- resu
      break
    }
  }

  message <- glue::glue_data(
    list(
      correct = final_result$correct,
      message = final_result$message
    ),
    {if (final_result$correct) correct else incorrect}
  )

  return(graded(
    correct = final_result$correct,
    message = message
  ))
}
