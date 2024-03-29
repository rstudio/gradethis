#' Grade student code against a solution (Legacy)
#'
#' `r lifecycle::badge("superseded")` Please use [grade_this_code()].
#'
#' Checks the code expression or the code result against a solution.
#'
#' `grade_code()` compares student code to a solution (i.e. model code) and
#' describes the first way that the student code differs. If the student code
#' exactly matches the solution, `grade_code()` returns a customizable success
#' message (`correct`). If the student code does not match the solution, a
#' customizable incorrect message (`incorrect`) can also be provided.
#'
#' `grade_code()` provides a *strict* check in that the student code must
#' exactly match the solution. It is not enough for the student code to be
#' equivalent to the solution code (e.g. to return the same result as the
#' solution).
#'
#' You can provide solution code for `grade_code()` to use in two ways:
#'
#' 1. Pass code as a character string or a quoted expression to the solution
#' argument of `grade_code()`
#'
#' 2. Make a "-solution" code chunk for the exercise to be checked in a learnr
#' document. There is no need to supply a solution argument for `grade_code()`
#' if you call it from the "-check" chunk of the same exercise. Likewise, there
#' is no need to supply a student submitted code argument when you call
#' `grade_code()` from a learnr document (learnr will provide the code that the
#' student submits when it runs `grade_code()`.
#'
#' For best results, name all arguments provided in the solution code.
#'
#' @param correct A character string to display if the student answer matches a
#'   known correct answer.
#'
#' @param incorrect A character string to display if the student answer matches
#'   a known incorrect answer.
#' @inheritParams code_feedback
#' @param grader_args A list of parameters passed to `grader` functions
#'   (provided by [gradethis_exercise_checker()]). This contains:
#'
#'   * `user_quo`: Quoted R code submitted by the user. For example
#'   \code{\link[rlang:quo]{rlang::quo(1)}}
#'
#'   * `solution_quo`: (Optional) Quoted solution R code provided by the
#'   `*-solution` chunk for an exercise.
#'
#' @param learnr_args A list of all parameters passed to
#'   [gradethis_exercise_checker()] by `learnr`. See
#'   <https://rstudio.github.io/learnr/exercises.html#exercise_checking> for
#'   more details.
#'
#' @param glue_correct A glue string that returns the final correct message
#'   displayed. Defaults to `getOption("gradethis_glue_correct")`.
#'
#' @param glue_incorrect A glue string that returns the final incorrect message
#'   displayed. Defaults to `getOption("gradethis_glue_incorrect")`.
#'
#' @param glue_pipe A glue string that returns the final message displayed when
#'   the student uses a pipe, `%>%`. Defaults to
#'   `getOption("gradethis.pipe_warning")`.
#' @param ... ignored. Should be empty
#'
#' @return a function whose first parameter should be an environment that contains
#' all necessary information to compare the code.  The result of the returned
#' function will be a [graded()] object. An incorrect message will describe the
#' first way that the answer differs, the message will be the content of the `glue_pipe`
#' argument.
#'
#' @seealso [grade_this_code()], [grade_code()]
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' gradethis_demo()
#' }
#'
#' # This is a manual example, see grading demo for `learnr` tutorial usage
#' y <- expression(sqrt(log(2)))
#' z <- expression(sqrt(log(1)))
#' grade_code(grader_args = list(user_quo = y, solution_quo = z))
grade_code <- function(
  correct = NULL,
  incorrect = NULL,
  ...,
  allow_partial_matching = getOption("gradethis.allow_partial_matching", TRUE),
  glue_correct = getOption("gradethis.glue_correct"),
  glue_incorrect = getOption("gradethis.glue_incorrect"),
  glue_pipe = deprecated(),
  grader_args = deprecated(),
  learnr_args = deprecated()
) {
  ellipsis::check_dots_empty()
  if (is_present(grader_args)) deprecate_warn("0.2.0", "grade_code(grader_args = )")
  if (is_present(learnr_args)) deprecate_warn("0.2.0", "grade_code(learnr_args = )")
  if (is_present(glue_pipe)) {
    deprecate_warn(
      "0.2.1",
      "grade_code(glue_pipe = )",
      details = paste(
        "Use the global option `gradethis.pipe_warning` to set the pipe warning message.",
        "Or use `\"{pipe_warning()}\"` in the `glue_correct` or `glue_incorrect` arguments of `grade_code()`.",
        sep = "\n"
      )
    )
  }


  # return script style function
  function(check_env) {
    if (is.list(check_env)) {
      check_env <- list2env(check_env)
    }

    user_code <- check_env$.user_code
    if (is.null(user_code)) {
      return(legacy_graded(
        correct = FALSE,
        message = "I didn't receive your code. Did you write any?"
      ))
    }

    solution_code_all <- check_env$.solution_code_all
    # If `.solution_code_all` is missing, make it from `.solution_code`
    if (is.null(solution_code_all) || length(str2expression(solution_code_all)) == 0) {
      solution_code_all <- solutions_prepare(check_env$.solution_code)
    }

    # If `.solution_code_all` is still missing, return an error
    if (is.null(solution_code_all) || length(str2expression(solution_code_all)) == 0) {
      return(legacy_graded(
        correct = FALSE,
        message = "No exercise solution provided. Defaulting to _incorrect_."
      ))
    }

    message <- code_feedback(
      user_code = user_code,
      solution_code = solution_code_all,
      user_env = check_env$.envir_result %||% check_env,
      solution_env = check_env$.envir_solution %||% check_env,
      allow_partial_matching = allow_partial_matching
    )

    if (is.null(message)) {
      # correct!
      return(legacy_graded(
        correct = TRUE,
        message = glue_message(
          glue_correct %||% gradethis_legacy_options$gradethis.glue_correct,
          .is_correct = TRUE,
          .message = NULL,
          .correct = correct,
          .user_code = user_code
        )
      ))
    }

    # is incorrect
    message <- glue_message(
      glue_incorrect %||% gradethis_legacy_options$gradethis.glue_incorrect,
      .is_correct = FALSE,
      .message = message,
      .incorrect = incorrect,
      .user_code = user_code
    )

    # final grade
    legacy_graded(correct = FALSE, message = message)
  }

}
