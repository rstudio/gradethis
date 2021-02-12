
#' Setup gradethis for use within learnr
#'
#' Use this function to change the default options suggested by gradethis. This
#' function also describes in detail each of the global options available for
#' customization in the gradethis package. Note that you most likely do not want
#' to change the defaults values for the learnr tutorial options that are
#' prefixed with `exercise.`. Each of the gradethis-specific arguments sets a
#' global option with the same name, prefixed with `gradethis.`. For example,
#' `pass` sets `gradethis.pass`.
#'
#' @examples
#' # Not run in package documentation because this function changes global opts
#' if (FALSE) {
#'   old_opts <- gradethis_setup(
#'     pass = "Great work!", 
#'     fail = "{random_encouragement()}"
#'   )
#' }
#' 
#' # Use getOption() to see the default value
#' getOption("gradethis.pass")
#' getOption("gradethis.code_correct")
#' 
#' @param pass Default message for [pass()]. Sets `options("gradethis.pass")`
#' @param fail Default message for [fail()]. Sets `options("gradethis.fail")`
#' @param code_correct Default `correct` message for [grade_this_code()]. If
#'   unset, [grade_this_code()] falls back to the value of the `gradethis.pass`
#'   option. Sets `options("gradethis.code_correct")`.
#' @param code_incorrect Default `incorrect` message for [grade_this_code()]. If
#'   unset [grade_this_code()] falls back to the value of the `gradethis.fail`
#'   option. Sets `options("gradethis.code_incorrect")`.
#' @param fail_code_feedback Logical `TRUE` or `FALSE` to determine whether 
#'   [maybe_code_feedback()] should return code feedback, where if `FALSE`,
#'   [maybe_code_feedback()] will return an empty string. This can be useful in
#'   [pass()] or [fail()] messages and is used by default in the default 
#'   [fail()] message and the default [grade_this_code()] incorrect message.
#' @param allow_partial_matching Logical `TRUE` or `FALSE` to determine whether
#'   partial matching is allowed in `grade_this_code()`. Sets
#'   `options("gradethis.allow_partial_matching")`.
#' @param pipe_warning The default message used in [pipe_warning()]. Sets
#'   `options("gradethis.pipe_warning")`.
#' @inheritParams learnr::tutorial_options
#' @inheritDotParams learnr::tutorial_options
#' 
#' @return Invisibly returns the global options as they were prior to setting
#'   them with `gradethis_setup()`.
#' 
#' @seealso [gradethis_exercise_checker()]
#' @export
gradethis_setup <- function(
  exercise.timelimit = NULL,
  exercise.checker = NULL,
  exercise.error.check.code = NULL,
  ...,
  pass = NULL,
  fail = NULL,
  code_correct = NULL,
  code_incorrect = NULL,
  fail_code_feedback = NULL,
  pipe_warning = NULL,
  allow_partial_matching = NULL
) {
  set_opts <- as.list(match.call()[-1])
  set_opts <- set_opts[setdiff(names(set_opts), "...")]
  
  learnr_opts <- names(gradethis_default_learnr_options)
  gradethis_opts <- names(gradethis_default_options)
  
  for (learnr_opt in learnr_opts) {
    if (learnr_opt %in% names(set_opts)) {
      do.call(learnr::tutorial_options, set_opts[learnr_opt])
    } else if (is.null(knitr::opts_chunk$get(learnr_opt)) || learnr_opt == "exercise.checker") {
      # Ensure that the default value is set
      knitr::opts_chunk$set(
        learnr_opt, 
        gradethis_default_learnr_options[[learnr_opt]]
      )
    }
  }
  
  if (length(list(...))) {
    learnr::tutorial_options(...)
  }
  
  old_opts <- options()
  
  # specifically set the options from this call
  set_gradethis_opts <- set_opts[setdiff(names(set_opts), learnr_opts)]
  if (length(set_gradethis_opts)) {
    # Won't need to check the default values of the explicitly set opts
    gradethis_opts <- setdiff(gradethis_opts, names(set_gradethis_opts))
    
    # Set the user-specified options
    names(set_gradethis_opts) <- paste0("gradethis.", names(set_gradethis_opts))
    options(set_gradethis_opts)
  }
  
  # Check that default values have been set
  if (length(gradethis_opts)) {
    needs_set <- !paste0("gradethis.", gradethis_opts) %in% names(old_opts)
    if (any(needs_set)) {
      gradethis_opts <- gradethis_opts[needs_set]
      set_gradethis_default <- gradethis_default_options[gradethis_opts]
      names(set_gradethis_default) <- paste0("gradethis.", gradethis_opts)
      options(set_gradethis_default)
    }
  }
  
  invisible(old_opts)
}
