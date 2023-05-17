.pipes <- c("%>%", "|>")

is_pipe <- function(x) {
  if (is.character(x)) x <- parse(text = x)[[1]]
  ifelse(is.call(x), as.character(x[[1]]) %in% .pipes, FALSE)
}

# From `?base::Syntax` but lightly organized into groups
.infixes_assign <- c("<-", "<<-", "->", "->>", "=", ":=")
.infixes_comp <- c("==", "!=", ">", ">=", "<", "<=")
.infixes_operator <- c(
  "::", ":::",
  "$", "@", "[", "[[", "^", "-", "+", ":", "*", "/",
  "!", "&", "&&", "|", "||", "~", "?"
)
.infixes <- c(
  "%%", "%/%", "%in%",
  .infixes_assign,
  .infixes_comp,
  .infixes_operator
)

is_infix <- function(x, infix_vals = .infixes, exact = FALSE) {

  tryCatch({
    if (is.character(x)) {
      out <- str2lang(x)
    } else {
      out <- x
    }

    if (!is.call(out)) {
      return(FALSE)
    }

    call_text <- as.character(out[[1]])

    is_known_infix <- any(call_text %in% infix_vals)
    if (exact) return(is_known_infix)

    is_known_infix || any(grepl("^%.*%$", call_text))
  }, error = function(e) {
    # x is not an infix
    FALSE
  })
}

is_infix_assign <- function(x) {
  is_infix(x, infix_vals = .infixes_assign, exact = TRUE)
}
