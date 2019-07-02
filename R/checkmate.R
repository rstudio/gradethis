chkm8_class <- checkmate::assert_class

chkm8_item_class <- function(x, class, name = checkmate::vname(x)) {
  lapply(seq_along(x), function(i) {
    chkm8_class(x[[i]], class, .var.name = paste0(name, "[[", i, "]]"))
  })
}

chkm8_single_character <- function(x, name = checkmate::vname(x)) {
  checkmate::assert_character(x, null.ok = TRUE, len = 1, any.missing = FALSE, .var.name = name)
}
