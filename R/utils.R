.pipes <- c("%>%")

is_pipe <- function(x) {
  if (is.character(x)) x <- parse(text = x)[[1]]
  ifelse(is.call(x), as.character(x[[1]]) %in% .pipes, FALSE)
}

.infixes_assign <- c("<-", "<<-", "->", "->>", "=")
.infixes_comp <- c("==", "!=", ">", ">=", "<", "<=")
.infixes <- c(
  "+", "-", "*", "/", "^", "$", "[", "[[", "!", "%%", "%/%", "%in%",
  .infixes_assign,
  .infixes_comp
)

is_infix <- function(x, infix_vals = .infixes) {

  tryCatch({
    if (is.character(x)) {
      out <- str2lang(x)
    } else {
      out <- x
    }

    if (!is.call(out)) {
      return(FALSE)
    }

    any(as.character(out[[1]]) %in% infix_vals)
  }, error = function(e) {
    # x is not an infix
    FALSE
  })
}

is_infix_assign <- function(x) {
  is_infix(x, infix_vals = .infixes_assign)
}

deparse_to_string <- function(x, width.cutoff = 500L, ...) {
  paste0(deparse(x, width.cutoff = width.cutoff, ...), collapse = "\n")
}


is_tag_like <- function(x) {
  inherits(x, c("shiny.tag", "shiny.tag.list"))
}

is_AsIs <- function(x) {
  inherits(x, "AsIs")
}

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
