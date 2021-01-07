gradethis_default_options <- list(

  # Default message for pass(message)
  gradethis.pass = "{ random_praise() } Correct!",
  # Default message for fail(message)
  gradethis.fail = "Incorrect.{ maybe_code_feedback() } { random_encouragement() }",

  # Default message for grade_this_code(correct)
  gradethis.code.correct = NULL,
  # Default message for grade_this_code(incorrect)
  gradethis.code.incorrect = "{.message_pipe_warning}{.message} {random_encouragement()}",

  # Default value for grade_this(fail_code_feedback). Plays with `maybe_code_feedback()`
  gradethis.code.feedback = TRUE,

  # Default value for grade_this_code(allow_partial_matching)
  gradethis.code.partial_matching = NULL,


  ### legacy ###
  gradethis_glue_correct = "{ random_praise() } { .message } { .correct }",
  gradethis_glue_incorrect = "{.message_pipe_warning}{ .message } { .incorrect } { random_encouragement() }",

  gradethis_glue_pipe = paste0(
    "I see that you are using pipe operators (e.g. %>%), ",
    "so I want to let you know that this is how I am interpretting your code ",
    "before I check it:\n\n```r\n{.user_code_unpiped}\n```\n\n"
  ),

  gradethis_glue_correct_test = "{ .num_correct }/{ .num_total } correct! { random_praise() }",
  gradethis_glue_incorrect_test = "{ .num_correct }/{ .num_total } correct! { random_encouragement() }"
)

# These values should mimic the values set in gradethis_setup()
gradethis_learnr_knitr_opts <- list(
  exercise.timelimit = 60,
  exercise.checker = grade_learnr,
  exercise.error.check.code = "grade_this_code()"
)

.onLoad <- function(libname, pkgname) {
  # initialize gradethis global options
  op <- options()
  toset <- !(names(gradethis_default_options) %in% names(op))
  if (any(toset)) options(gradethis_default_options[toset])

  # initialize tutorial options
  if ("knitr" %in% loadedNamespaces()) {
    Map(
      names(gradethis_learnr_knitr_opts),
      gradethis_learnr_knitr_opts,
      f = function(key, val) {
        # only set if the current value is NULL
        if (is.null(knitr::opts_chunk$get(key))) {
          knitr::opts_chunk$set(key, val)
        }
      }
    )
  }

  invisible()
}
