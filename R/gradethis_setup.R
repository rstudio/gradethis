

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
#' @section Global package options:
#' 
#'   ```{r child="man/fragments/gradethis-setup.Rmd"}
#'   ```
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
#' getOption("gradethis.maybe_code_feedback")
#' 
#' @param pass Default message for [pass()]. Sets `options("gradethis.pass")`
#' @param fail Default message for [fail()]. Sets `options("gradethis.fail")`
#' @param code_correct Default `correct` message for [grade_this_code()]. If
#'   unset, [grade_this_code()] falls back to the value of the `gradethis.pass`
#'   option. Sets the `gradethis.code_correct` option.
#' @param code_incorrect Default `incorrect` message for [grade_this_code()]. If
#'   unset [grade_this_code()] falls back to the value of the `gradethis.fail`
#'   option. Sets the `gradethis.code_incorrect` option.
#' @param maybe_code_feedback Logical `TRUE` or `FALSE` to determine whether 
#'   [maybe_code_feedback()] should return code feedback, where if `FALSE`,
#'   [maybe_code_feedback()] will return an empty string.
#'   [maybe_code_feedback()] is used in the default messages when [pass()] or 
#'   [fail()] are called without any arguments, which are set by the `pass` or
#'   `fail` arguments of [gradethis_setup()].
#' @param maybe_code_feedback.before,maybe_code_feedback.after Text that should
#'   be added `before` or `after` the `maybe_code_feedback()` output, if any is
#'   returned. Sets the default values of the `before` and `after` arguments of
#'   [maybe_code_feedback()].
#' @param pass.praise Logical `TRUE` or `FALSE` to determine whether a praising
#'   phrase should be automatically prepended to any [pass()] or
#'   [pass_if_equal()] messages. Sets the `gradethis.pass.praise` option.
#' @param fail.hint Logical `TRUE` or `FALSE` to determine whether an automated
#'   code feedback hint should be shown with a [fail()] or [fail_if_equal()]
#'   message. Sets the `gradethis.fail.hint` option.
#' @param fail.encourage Logical `TRUE` or `FALSE` to determine whether an
#'   encouraging phrase should be automatically appended to any [fail()] or
#'   [fail_if_equal()] messages. Sets the `gradethis.fail.encourage` option.
#' @param allow_partial_matching Logical `TRUE` or `FALSE` to determine whether
#'   partial matching is allowed in `grade_this_code()`. Sets the
#'   `gradethis.allow_partial_matching` option.
#' @param pipe_warning The default message used in [pipe_warning()]. Sets the
#'   `gradethis.pipe_warning` option.
#' @param fail_code_feedback Deprecated. Use `maybe_code_feedback`.
#' @inheritParams learnr::tutorial_options
#' @inheritDotParams learnr::tutorial_options
#' 
#' @return Invisibly returns the global options as they were prior to setting
#'   them with `gradethis_setup()`.
#' 
#' @seealso [gradethis_exercise_checker()]
#' @export
gradethis_setup <- function(
  pass = NULL,
  fail = NULL,
  ...,
  code_correct = NULL,
  code_incorrect = NULL,
  maybe_code_feedback = NULL,
  maybe_code_feedback.before = NULL,
  maybe_code_feedback.after = NULL,
  pass.praise = NULL,
  fail.hint = NULL,
  fail.encourage = NULL,
  pipe_warning = NULL,
  allow_partial_matching = NULL,
  exercise.checker = gradethis_exercise_checker,
  exercise.timelimit = NULL,
  exercise.error.check.code = NULL,
  fail_code_feedback = NULL
) {
  if (isTRUE(getOption("gradethis.__require__", TRUE))) {
    # avoids cyclical loading when called by .onLoad(). Even if called as
    # gradethis::gradethis_setup(), .onLoad() is called first, setting the
    # default option values ahead of the current gradethis_setup() call
    require(gradethis)
  }
  
  
  set_opts <- as.list(match.call()[-1])
  set_opts <- lapply(set_opts, eval, envir = new.env())
  set_opts <- set_opts[setdiff(names(set_opts), "...")]
  
  if (!is.null(fail_code_feedback)) {
    lifecycle::deprecate_warn(
      when = "0.2.3", 
      what = "gradethis_setup(fail_code_feedback=)", 
      with = "gradethis_setup(maybe_code_feedback=)"
    )
    if (missing(maybe_code_feedback)) {
      set_opts[["maybe_code_feedback"]] <- fail_code_feedback
      set_opts[["fail_code_feedback"]] <- NULL
    }
  }
  
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


# Default Options ---------------------------------------------------------

gradethis_default_options <- list(
  
  # Default message for pass(message)
  pass = "{random_praise()} Correct!",
  pass.praise = FALSE,
  # Default message for fail(message)
  fail = "Incorrect.{maybe_code_feedback()} {random_encouragement()}",
  fail.hint = FALSE,
  fail.encourage = FALSE,
  
  # Default value for grade_this(maybe_code_feedback). Plays with `maybe_code_feedback()`
  maybe_code_feedback = TRUE,
  maybe_code_feedback.before = " ",
  maybe_code_feedback.after = NULL,
  
  # Default message for grade_this_code(correct)
  code_correct = NULL,
  # Default message for grade_this_code(incorrect)
  code_incorrect = "{pipe_warning()}{code_feedback()} {random_encouragement()}",
  # Default message used for pipe_warning()
  pipe_warning = paste0(
    "I see that you are using pipe operators (e.g. %>%), ",
    "so I want to let you know that this is how I am interpretting your code ",
    "before I check it:\n\n```r\n{.user_code_unpiped}\n```\n\n"
  ),
  
  # Default value for grade_this_code(allow_partial_matching)
  allow_partial_matching = NULL
)

# Legacy Options ----------------------------------------------------------

gradethis_legacy_options <- list(
  ### legacy ###
  glue_correct = "{random_praise()} {.message} {.correct}",
  glue_incorrect = "{pipe_warning()}{.message} {.incorrect} {random_encouragement()}",
  
  
  glue_correct_test = "{.num_correct}/{.num_total} correct! {random_praise()}",
  glue_incorrect_test = "{.num_correct}/{.num_total} correct! {random_encouragement()}"
)

names(gradethis_legacy_options) <- paste0(
  "gradethis.", names(gradethis_legacy_options)
)

# Default learnr Options --------------------------------------------------

gradethis_default_learnr_options <- list(
  exercise.timelimit = 60,
  exercise.checker = gradethis_exercise_checker,
  exercise.error.check.code = "grade_this_code()"
)
