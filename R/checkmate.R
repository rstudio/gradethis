chkm8_class <- checkmate::assert_class

chkm8_item_class <- function(x, class, name = checkmate::vname(x)) {
  lapply(seq_along(x), function(i) {
    chkm8_class(x[[i]], class, .var.name = paste0(name, "[[", i, "]]"))
  })
}

chkm8_single_character <- function(x, name = checkmate::vname(x), null.ok = TRUE, any.missing = FALSE, ...) { # nolint: object_name
  checkmate::assert_character(x, null.ok = null.ok, len = 1, any.missing = any.missing, .var.name = name, ...)
}

chm8_single_atomic <- function(x, name = checkmate::vname(x)) {
  checkmate::assert_atomic(x, min.len = 0, max.len = 1, any.missing = TRUE, .var.name = name)
}
