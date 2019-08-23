#' Grade all specified conditions
#'
#' Executes tests against the final result of the user code. If
#' a test throws an error, the test fails and the submitted answer will be
#' marked incorrect.
#'
#' Executes tests against the final result of the user code. If
#' a test throws an error, the test fails and the submitted answer will be
#' marked incorrect.
#'
#' @inheritParams grade_code
#'
#' @param correct A character string to display if all tests pass. This
#'   character string will be run through [glue::glue_data] with:
#'
#'   * `num_correct`: Number of correct tests. (Equals `num_total`)
#'
#'   * `num_total`: Number of tests
#'
#'   * `errors`: Vector of errors found. (`NULL`)
#'
#' @param incorrect A character string to display if at least one test fails.
#'   This character string will be run through [glue::glue_data()] with:
#'
#'   * `num_correct`: Number of correct tests
#'
#'   * `num_total`: Number of tests
#'
#'   * `errors`: Vector of errors found
#'
#' @param ... ignored
#'
#'
#' @return a `grader_graded` structure from [graded()] containing a formatted
#'   `correct` or `incorrect` message.
#' @seealso `test`
#' @export
#' @examples
#' \dontrun{gradethis_demo()}
#'
#' example_function <- function(x){
#'   return(x + 1)
#' }
#' grade_conditions(
#'   pass_if(~ .result(3) == 4),
#'   pass_if(~ .result(10) == 11),
#'   grader_args = list(),
#'   learnr_args = list(last_value = example_function, envir_prep = new.env())
#' )
#'
#' grade_conditions(
#'   pass_if(~ .result(3) == 4),
#'   fail_if(~ .result(10) == 11),
#'   grader_args = list(),
#'   learnr_args = list(last_value = example_function, envir_prep = new.env())
#' )
#'
grade_conditions <- function(
  ...,
  correct = NULL,
  incorrect = NULL,
  grader_args = list(),
  learnr_args = list(),
  glue_correct = getOption("gradethis_glue_correct_test"),
  glue_incorrect = getOption("gradethis_glue_incorrect_test")
) {

  conditions <- list(...)
  chkm8_item_class(conditions, "grader_condition")

  test_results <- purrr::map(conditions, pass_fail_condition_modify,
                             grader_args = grader_args,
                             learnr_args = learnr_args)

  condi_correct_status <- sapply(test_results, function(x) x$correct)
  num_correct <- sum(condi_correct_status)

  if (num_correct == length(conditions)) {
    final_result <- graded(correct = TRUE, message = NULL)
  } else {
    final_result <- graded(correct = FALSE, message = NULL)
  }

  message <- glue_message(
    {if (final_result$correct) glue_correct else glue_incorrect}, # nolint
    .is_correct = final_result$correct,
    .message = final_result$message,
    .correct = correct,
    .incorrect = incorrect,
    .num_correct = as.character(num_correct),
    .num_total = as.character(length(conditions))
  )

  ret <- graded(
    correct = final_result$correct,
    message = message
  )

  return(ret)
}

#' helper function used in test_result
#'
#' test_result is very similar to check_result.
#' in check_result, we just go through all the cases and once one of the
#' conditions match, we are done.
#' However, in test_result, we need to go though all the conditions
#' and store their values to tally up the total number of "good" passing cases.
#' It's behaviour is similar to running a unit testing suite,
#' all the cases need to be run and tallied up in the end.
#'
#' Since the API for test_result uses pass_if and fail_if,
#' we are presented with another problem,
#' When a pass_if condition is found (i.e., matched and returns TRUE),
#' that means the test is "passing".
#' However, when a fail_if condition is found,
#' that means the test is actually "failing",
#' We do not need to flip the `correct` condition in the graded object,
#' because fail_if returns correct = FALSE
#'
#' This function just flips that boolean condition given depending on
#' whether a pass_if or fail_if condition is passing or failing.
#'
#' This way we can store all the graded$correct values into a boolean vector.
#' In order to calculate the number of passing conditions,
#' we can then sum the boolean vector.
#' @noRd
pass_fail_condition_modify <- function(condi, grader_args, learnr_args){
  evaluated_condi <- evaluate_condition(condi, grader_args, learnr_args)

  # need to account for the case when fail_if does not match (this means the test passed)
  # so we would need to flip the graded class correct status
  if (condi$correct) { # evaluating a pass_if condition # nolint
    # if a pass_if returns a NULL, it means the condition evaluated FALSE, which is a bad thing
    # if it is NULL, we give it an "incorrect" graded class value
    # if it is "correct", we keep the "correct" graded class value
    evaluated_condi <- evaluated_condi %||% graded(correct = FALSE, message = NULL)
    return(evaluated_condi)

  } else { # evaluating a fail_if condition # nolint
    # if a fail_if returns NULL, it means the condition evaluated FALSE, which is a good thing
    # if it is NULL, we give it a "correct" graded class value
    # a passing fail_if is bad thing, we don't need to flip becuase it reutnrs correct = FALSE
    evaluated_condi <- evaluated_condi %||% graded(correct = TRUE, message = NULL)
    return(evaluated_condi)
  }
}
