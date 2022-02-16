
local_options_waldo_compare <- function(.local_envir = parent.frame()) {
  # These options are set by fansi and diffobj (used by waldo::compare()) but
  # may be unset by learnr when it reset options after running student & grading
  # code. If they aren't set, the underlying packages throw errors.
  #
  # This work-around is worth the effort because waldo::compare() does a great
  # deal of work to compare many different types of R objects. The downside is
  # that its output is a user-facing message and not a T/F or result value.
  waldo_opts <- list(
    fansi.warn = FALSE,
    diffobj.warn = FALSE,
    diffobj.max.diffs = 10L
  )

  if (identical(.local_envir, globalenv())) {
    return(invisible(options(waldo_opts)))
  }

  withr::local_options(waldo_opts, .local_envir = .local_envir)
}

local_knitr_opts_chunk <- function (new = list(), .local_envir = parent.frame()) {
  old <- knitr::opts_chunk$get()
  knitr::opts_chunk$set(new)

  withr::defer(envir = .local_envir, {
    knitr::opts_chunk$set(old)
    new_opts <- setdiff(names(new), names(old))
    for (opt in new_opts) {
      # restore opts that were created in `new`
      knitr::opts_chunk$restore(opt)
    }
  })

  invisible(old)
}

with_knitr_opts_chunk <- function(new, expr) {
  local_knitr_opts_chunk(new)
  force(expr)
}
