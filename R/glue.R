#' @noRd
glue_message <- function(
  msg,
  ...
) {
  params = list(...)
  param_names <- names(params)
  is_bool <- grepl("^\\.is_", param_names)
  bool_names <- param_names[is_bool]
  char_names <- param_names[!is_bool]

  if (length(bool_names) > 1) {
    params[bool_names] <- lapply(params[bool_names], function(x){x %||% NA})
  }
  
  params[char_names] <- lapply(char_names, function(char_name) {
    x <- params[[char_name]] %||% ""
    chkm8_single_character(x, char_name)
    x
  })

  glue::glue_data(params, msg)
}
