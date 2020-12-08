#' Grade result of exercise code (Legacy)
#'
#' \lifecycle{superseded} Please use [grade_this()] mixed with [pass()], [pass_if_equal()], [fail()], and/or [fail_if_equal()].
#'
#' `grade_result()` and `grade_result_strict()` both take a set of `pass_if()`/`fail_if()`
#' conditions, evaluate them, and return a final [graded()] object. For `grade_result_strict()`
#' to return a correct grade, every `pass_if()` condition must be met, and every `fail_if()` condition
#' must not be met. On the other hand, `grade_result()`'s final grade reflects the first satisfied
#' condition (if no conditions are met, the final grade can be controlled by `default_correct` and
#' `default_message`).
#'
#' @inheritParams grade_code
#' @param ... `pass_if()`/`fail_if()` `condition()`s to check.
#' @param default_correct In the event that no [condition()]s are met, should the end result
#'   be correct? When `"auto"`, this will be `TRUE` when all the [conditions()] are [fail_if()]
#'   (and `FALSE` otherwise).
#' @param default_message In the event that no [condition()]s are met, what message should be
#'   included with the returned [graded()] object?
#'
#' @return a function whose first parameter should be an environment that contains
#' all necessary information to compare the user's result.  The result of the returned function
#' will be a [graded()] object containing a formatted `correct` or `incorrect` message.
#'
#' @seealso [grade_code()]
#' @keywords internal
#' @export
#' @examples
#'
#' grade_result(
#'   fail_if(~ identical(.result, 4), "Try adding 1"),
#'   pass_if(~ identical(.result, 5), "You got 5, great!"),
#'   fail_if(~ TRUE, "Some generic failing message.")
#' )(list(.last_value = 5, .envir_result = new.env()))
#'
#' grade_result_strict(
#'   pass_if(~ identical(.result, 5), "You got 5, great!"),
#'   fail_if(~ !is.integer(.result), "I expected an integer")
#' )(list(.last_value = 5, .envir_result = new.env()))
#'
#' grade_result(
#'   fail_if(~ !is.function(.result), "I expected a function."),
#'   fail_if(~ .result(1) != 2, "Your function should add one.")
#' )(list(.last_value = function(x) {x + 2}, .envir_result = new.env()))
#'
#' # To learn more about using grade_result() and grade_code() with learnr, see:
#' \dontrun{gradethis_demo()}
grade_result <- function(
  ...,
  correct = NULL,
  incorrect = NULL,
  glue_correct = getOption("gradethis_glue_correct"),
  glue_incorrect = getOption("gradethis_glue_incorrect"),
  default_correct = "auto",
  default_message = NULL,
  grader_args = deprecated(),
  learnr_args = deprecated()
) {

  if (is_present(grader_args)) deprecate_warn("0.2.0", "grade_result(grader_args = )")
  if (is_present(learnr_args)) deprecate_warn("0.2.0", "grade_result(learnr_args = )")

  conditions <- list(...)
  if (!length(conditions)) {
    stop("At least one condition object (e.g., `pass_if()`, `fail_if()`, `condition()`) must be provided to `grade_result()`", call. = FALSE)
  }
  chkm8_item_class(conditions, "gradethis_condition")

  # If there is at least one pass_if() condition, then default to an incorrect grade;
  # otherwise, default to a correct grade https://github.com/rstudio-education/gradethis/issues/118
  if (identical(default_correct, "auto")) {
    default_correct <- !any(vapply(conditions, `[[`, logical(1), "correct"))
  }
  chkm8_class(default_correct, "logical")

  # return a script style function
  function(check_env) {
    last_value <- check_env$.last_value
    env <- learnr_env(envir_prep = check_env$.envir_prep, envir_result = check_env$.envir_result)

    final_grade <- legacy_graded(correct = default_correct, message = default_message)
    found_grade <- FALSE
    for (cond in conditions) {
      grade <- evaluate_condition(
        cond,
        last_value = last_value,
        env = env
      )
      if (length(grade)) {
        final_grade <- grade
        found_grade <- TRUE
        break
      }
    }

    legacy_graded(
      correct = final_grade$correct,
      message = glue_message(
        if (final_grade$correct) glue_correct else glue_incorrect, # nolint
        .is_match = found_grade,
        .is_correct = final_grade$correct,
        .message = final_grade$message,
        .correct = correct,
        .incorrect = incorrect
      )
    )
  }

}


#' @rdname grade_result
#' @export
#' @inheritParams grade_code
grade_result_strict <- function(
  ...,
  correct = NULL,
  incorrect = NULL,
  glue_correct = getOption("gradethis_glue_correct_test"),
  glue_incorrect = getOption("gradethis_glue_incorrect_test"),
  grader_args = deprecated(),
  learnr_args = deprecated()
) {
  if (is_present(grader_args)) deprecate_warn("0.2.0", "grade_result_strict(grader_args = )")
  if (is_present(learnr_args)) deprecate_warn("0.2.0", "grade_result_strict(learnr_args = )")

  conditions <- list(...)
  chkm8_item_class(conditions, "gradethis_condition")

  # return a script style function
  function(check_env) {
    last_value <- check_env$.last_value
    env <- learnr_env(envir_prep = check_env$.envir_prep, envir_result = check_env$.envir_result)

    grades <- lapply(conditions, function(x) {
      res <- evaluate_condition(x, last_value = last_value, env = env)
      # If a pass_if() condition isn't matched (i.e. res is NULL), then
      # it should be considered an incorrect result.
      res %||% legacy_graded(correct = !x$correct)
    })

    num_correct <- sum(vapply(grades, function(x) x$correct, logical(1)))
    is_correct <- num_correct == length(conditions)

    legacy_graded(
      correct = is_correct,
      message = glue_message(
        if (is_correct) glue_correct else glue_incorrect, # nolint
        .is_correct = is_correct,
        .message = NULL,
        .correct = correct,
        .incorrect = incorrect,
        .num_correct = as.character(num_correct),
        .num_total = as.character(length(conditions))
      )
    )
  }

}




#' @rdname grade_result
#' @export
#' @param x A formula, function, or value, that returns `TRUE` or `FALSE`.
#'    When comparing objects that are greater than length 1
#'    (e.g., vectors, dataframes, matricies, etc)
#'    A logical vector will be returned if the user uses `==`, not a single logical value.
#'    `gradethis` will run the vector through
#'     `all(..., na.rm = TRUE)` to check for the logical value.
#'    It is advised that the user use `identical()` instead of `==` in this case.
#' @param message character string for message returned (usually passed in from
#'    [pass_if()] or [fail_if()].
#' @param correct logical whether the condition is the correct answer.
condition <- function(x, message, correct) {
  stopifnot(length(correct) == 1)
  chkm8_item_class(correct, "logical")

  structure(
    list(
      x = x,
      message = message,
      correct = correct,
      type = if (rlang::is_formula(x)) {
        "formula"
      } else if (rlang::is_function(x)) {
        "function"
      } else {
        "value"
      }
    ),
    class = "gradethis_condition"
  )
}

#' @rdname grade_result
#' @export
pass_if <- function(x, message = NULL) {
  condition(x, message, correct = TRUE)
}

#' @rdname grade_result
#' @export
fail_if <- function(x, message = NULL) {
  condition(x, message, correct = FALSE)
}



learnr_env <- function(envir_prep, envir_result) {
  envir_result %||%
    envir_prep %||%
    stop("Internal error. learnr did not pass a relevant environment")
}
