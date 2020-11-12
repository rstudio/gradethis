.pipes <- c("%>%")

is_pipe <- function(x) {
  if (is.character(x)) x <- parse(text = x)[[1]]
  ifelse(is.call(x), as.character(x[[1]]) %in% .pipes, FALSE)
}

.infixes_assign <- c("<-", "<<-", "->", "->>", "=")
.infixes <- c("+", "-", "*", "/", "^", "%%", "%/%", "%in%", .infixes_assign)

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



with_options <- function(opts, expr) {
  old_opts <- options(opts)
  on.exit(options(old_opts), add = TRUE)
  force(expr)
}




is_infix_assign <- function(x) {
  is_infix(x, infix_vals = .infixes_assign)
}

deparse_to_string <- function(x, width.cutoff = 500L, ...) {
  paste0(deparse(x, width.cutoff = width.cutoff, ...), collapse = "\n")
}
