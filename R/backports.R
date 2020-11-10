
# added in R 3.6
str2expression <- function(text) {
  parse(text = text, keep.source = FALSE)
}
str2lang <- function(text) {
  str2expression(text)[[1]]
}
