# repipe <- function(lst, .call = FALSE) {
#   text <- purrr::map(lst, deparse)
#   text <- purrr::reduce(text, paste, sep = " %>% ")
#   text <- gsub("\\(NULL\\)", "()", text)

#   if (.call) parse(text = text)[[1]]
#   else text
# }
