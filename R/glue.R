#' Generate Glue string from expression
#'
#' Checks and validates arguments passed into `glue` to generate messages to the student.
#'
#' @param glue_expression A glue character expression string.
#' @param ... Values to be inserted into glue expression.
#' @noRd
glue_message <- function(
  glue_expression,
  ...
) {
  params <- list(...)
  param_names <- names(params)
  is_bool <- grepl("^\\.is_", param_names)
  bool_names <- param_names[is_bool]
  char_names <- param_names[!is_bool]

  if (length(bool_names) > 1) {
    params[bool_names] <- lapply(params[bool_names], function(x){x %||% NA}) # nolint
  }

  params[char_names] <- lapply(char_names, function(char_name) {
    x <- params[[char_name]] %||% "" # convert NULL strings to "" to work with glue
    chkm8_single_character(x, char_name)
    x
  })

  ret <- glue::glue_data(params, glue_expression)
  return(ret)
}
