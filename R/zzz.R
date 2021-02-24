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
