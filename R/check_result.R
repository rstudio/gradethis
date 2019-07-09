
#' Check result of exercise code
#'
#' \code{check_result()} compares the final result of the user code to known \code{results}.
#' If the user result exactly matches a known \code{result}, \code{check_result}
#' returns the matching message value.
#'
#' @template correct
#' @param incorrect A character string to display if the student answer matches
#'   a known answer.
#'   This character string will be run through
#'   \code{glue::\link[glue]{glue_data}} with
#'   \code{list(correct = FALSE, message = "<result message>")}.
#'   where message is the matched result message.
#' @template grader_args
#' @template learnr_args
#' @param ... ignored
#'
#' @return a \code{grader_graded} structure from either
#'   \code{\link{pass_if}} or \code{\link{fail_if}} containing a formatted
#'   \code{correct} or \code{incorrect} message and whether or not a match was found.
#'
#' @export
#' @examples
#' \dontrun{grading_demo()}
check_result <- function(
  ...,
  correct = "{paste0(random_praise(), if (nchar(message) > 0) \" \", message)}",
  incorrect = "{paste0(message, if(nchar(message) > 0) \" \", random_encourage())}",
  grader_args = list(), # provided by `grade_learnr`
  learnr_args = list() # provided by `grade_learnr`
) {
  results <- list(...)
  chkm8_item_class(results, "grader_condition")
  chkm8_single_character(correct)
  chkm8_single_character(incorrect)

  if (!any(vapply(results, `[[`, logical(1), "correct"))) {
    stop("At least one correct result must be provided")
  }

  # init final answer as not found
  final_result <- graded(correct = FALSE, NULL)
  found_match <- FALSE

  for (resu in results) {
    evaluated_condi <- evaluate_condi(resu, grader_args, learnr_args)
    if (! is.null(evaluated_condi)) {
      return(
        graded(correct = evaluated_condi$correct,
               message = evaluated_condi$message)
      )
    }
  }

  message <- glue::glue_data(
    list(
      matched = found_match,
      correct = final_result$correct,
      message = final_result$message
    ),
    {if (final_result$correct) correct else incorrect} # nolint
  )

  return(graded(
    correct = final_result$correct,
    message = message
  ))
}
