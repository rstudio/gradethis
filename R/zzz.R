gradethis_default_options <- list(
  gradethis_glue_correct = "{ random_praise() } { .message } { .correct }",
  gradethis_glue_incorrect = "{ .message } { .incorrect } { random_encourage() }",

  # nolint start
  gradethis_glue_pipe = "I see that you are using pipe operators (e.g. %>%), 
      so I want to let you know that this is how I am interpretting your code 
      before I check it:\n\n{deparse(unpipe_all(.user))}\n\n{.message}
      "
  # nolint end
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(gradethis_default_options) %in% names(op))
  if (any(toset)) options(gradethis_default_options[toset])

  invisible()
}
