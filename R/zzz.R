gradethis_default_options <- list(

  gradethis.pass = "{ random_praise() } Correct!",
  gradethis.fail = "Incorrect. { random_encourage() }",

  gradethis.code.correct = NULL,
  gradethis.code.incorrect = "{.message} {random_encouragement()}",


  ### legacy ###
  gradethis_glue_correct = "{ random_praise() } { .message } { .correct }",
  gradethis_glue_incorrect = "{ .message } { .incorrect } { random_encourage() }",

  gradethis_glue_pipe = paste0(
    "I see that you are using pipe operators (e.g. %>%), ",
    "so I want to let you know that this is how I am interpretting your code ",
    "before I check it:\n\n{deparse_to_string(unpipe_all(.user), 60)}\n\n{.message}"),

  gradethis_glue_correct_test = "{ .num_correct }/{ .num_total } correct! { random_praise() }",
  gradethis_glue_incorrect_test = "{ .num_correct }/{ .num_total } correct! { random_encourage() }"
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(gradethis_default_options) %in% names(op))
  if (any(toset)) options(gradethis_default_options[toset])

  invisible()
}
