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
