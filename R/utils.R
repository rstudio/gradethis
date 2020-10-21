.pipes <- c("%>%")

#' @importFrom rlang %||%
NULL

is_pipe <- function(x) {
  if (is.character(x)) x <- parse(text = x)[[1]]
  ifelse(is.call(x), as.character(x[[1]]) %in% .pipes, FALSE)
}

.infixes_assign <- c("<-", "<<-", "->", "->>", "=")
.infixes <- c("+", "-", "*", "/", "^", "%%", "%/%", "%in%", .infixes_assign)

is_infix <- function(x, infix_vals = .infixes) {
  
  # if str2lang fails, x is not an infix
  out <- x
  if (is.character(x)){out<-"";  try(out <- str2lang(x), silent=TRUE) } 
  # if (is.character(x)) x <- parse(text = x)[[1]]
  ifelse(is.call(out), as.character(out[[1]]) %in% infix_vals, FALSE)
}




is_infix_assign <- function(x) {
  is_infix(x, infix_vals = .infixes_assign)
}

deparse_to_string <- function(x, width.cutoff = 500L, ...) {
  paste0(deparse(x, width.cutoff = width.cutoff, ...), collapse = "\n")
}