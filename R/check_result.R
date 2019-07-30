
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
#' @param glue_correct A glue string that returns the final correct message displayed.
#'    Defaults to getOption("gradethis_glue_correct").
#' @param glue_incorrect A glue string that returns the final incorrect message displayed.
#'    Defaults to getOption("gradethis_glue_incorrect").
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
  correct = NULL,
  incorrect = NULL,
  grader_args = list(),
  learnr_args = list(),
  glue_correct = getOption("gradethis_glue_correct"),
  glue_incorrect = getOption("gradethis_glue_incorrect")
) {
  # convert NULL correct/incorrect strings to "" to work with glue
  if (is.null(correct)) {correct <- ""}      # nolint
  if (is.null(incorrect)) {incorrect <- ""}  # nolint

  results <- list(...)
  chkm8_item_class(results, "grader_condition")
  chkm8_single_character(correct)
  chkm8_single_character(incorrect)

  if (!any(vapply(results, `[[`, logical(1), "correct"))) {
    stop("At least one correct result must be provided")
  }

  # init final answer as not found
  final_result <- graded(correct = FALSE, message = NULL)
  found_match <- FALSE

  for (resu in results) {
    evaluated_condi <- evaluate_condition(resu, grader_args, learnr_args)
    if (! is.null(evaluated_condi)) {
      final_result <- evaluated_condi
      found_match <- TRUE
      break
    }
  }

  message <- glue::glue_data(
    list(
      .is_match = found_match,
      .is_correct = final_result$correct,
      .message = final_result$message,
      .correct = correct,
      .incorrect = incorrect
    ),
    {if (final_result$correct) glue_correct else glue_incorrect} # nolint
  )

  return(graded(
    correct = final_result$correct,
    message = message
  ))
}
