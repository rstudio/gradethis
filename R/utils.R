.pipes <- c("%>%")

is_pipe <- function(x) {
  if (is.character(x)) x <- parse(text = x)[[1]]
  ifelse(is.call(x), as.character(x[[1]]) %in% .pipes, FALSE)
}

.infixes <- c("+", "-", "*", "/", "^", "%%", "%/%", "%in%")

is_infix <- function(x) {
  if (is.character(x)) x <- parse(text = x)[[1]]
  ifelse(is.call(x), as.character(x[[1]]) %in% .infixes, FALSE)
}