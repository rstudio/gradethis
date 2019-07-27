gradethis_default_options <- list(
  gradethis_glue_correct = "{ random_praise() } { .message } { .correct }",
  gradethis_glue_incorrect = "{ .message } { .incorrect } { random_encourage() }"
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(gradethis_default_options) %in% names(op))
  if (any(toset)) options(gradethis_default_options[toset])

  invisible()
}
