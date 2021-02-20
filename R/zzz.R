
# Default Options ---------------------------------------------------------

gradethis_default_options <- list(

  # Default message for pass(message)
  pass = "{random_praise()} Correct!",
  # Default message for fail(message)
  fail = "Incorrect.{maybe_code_feedback()} {random_encouragement()}",
  
  # Default value for grade_this(maybe_code_feedback). Plays with `maybe_code_feedback()`
  maybe_code_feedback = TRUE,

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


# Set It All Up onLoad ----------------------------------------------------

.onLoad <- function(libname, pkgname) {
  # Ensure default learnr and gradethis opts are set
  with_options(
    list("gradethis.__require__" = FALSE),
    gradethis_setup(
      # Need to call these explicitly because we want to overwrite
      # the values that were already in use, if any
      exercise.timelimit = gradethis_default_learnr_options$exercise.timelimit,
      exercise.checker = gradethis_default_learnr_options$exercise.checker,
      exercise.error.check.code = gradethis_default_learnr_options$exercise.error.check.code
    )
  )
  
  # Set legacy options onLoad as these aren't exposed in gradethis_setup()
  toset <- !names(gradethis_legacy_options) %in% names(options())
  if (any(toset)) {
    options(gradethis_legacy_options[toset])
  }
  

  invisible()
}
