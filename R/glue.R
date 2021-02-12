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
  if (is_tag_like(glue_expression) || is_AsIs(glue_expression)) {
    return(glue_expression)
  }
  
  params <- list(...)
  param_names <- names(params)
  is_bool <- grepl("^\\.is_", param_names)
  is_numb <- grepl("^\\.num_", param_names)

  bool_names <- param_names[is_bool]
  numb_names <- param_names[is_numb]
  char_names <- param_names[!(is_bool | is_numb)]

  if (length(bool_names) > 1) {
    params[bool_names] <- lapply(params[bool_names], function(x){x %||% NA}) # nolint
  }

  if (length(numb_names) > 1) {
    params[numb_names] <- lapply(params[numb_names], function(x){x %||% NA}) # nolint
  }

  params[char_names] <- lapply(params[char_names], function(x){x %||% ""}) # nolint

  params <- purrr::map_chr(params, ~ paste(as.character(.x), collapse = ""))
  purrr::walk(params, chm8_single_atomic, name = param_names)
  purrr::walk(params[char_names], chkm8_single_character, name = char_names)

  ret <- glue::glue_data(params, glue_expression)
  return(ret)
}

glue_message_with_env <- function(env, message) {
  if (is_tag_like(message) || is_AsIs(message)) {
    return(message)
  }
  if (length(message) > 1) {
    message <- paste(as.character(message), collapse = "")
  }
  glue_with_env(env, message)
}

glue_with_env <- function(env, ...) {
  glue::glue_data(.x = env, .envir = env, ...)
}

#' Pipe Warning Message
#' 
#' Creates a warning message when user code contains the `%>%`.
#' 
#' @param message A glue string containing the message
#' @param .user_code The user's submitted code, found in `env` if `NULL`
#' 
#' @export
pipe_warning <- function(
  message = getOption("gradethis.pipe_warning"),
  .user_code = NULL
) {
  if (is.null(message)) {
    return("")
  }
  
  if (is.null(.user_code)) {
    .user_code <- get0(".user_code", parent.frame(), ifnotfound = "")
  }
  
  if (identical(trimws(.user_code), "") || !uses_pipe(.user_code)) {
    return("")
  }
  
  # for compatibility allow .user and .message but print a warning to the console
  .user <- function() {
    message("{.user} was deprecated in `pipe_warning()`, please use {.user_code}.")
    .user_code
  }
  
  .message <- function() {
    message("{.message} was deprecated in `pipe_warning()`.")
    ""
  }
  
  .user_code_unpiped <- .user_code
  if (!identical(.user_code, "")) {
    # convert forwards and backwards to apply consistent formatting
    .user_code <- as.character(str2expression(.user_code))
    .user_code_unpiped <- unpipe_all_str(.user_code, width = 60)
  }
  
  glue::glue(
    message,
    .user_code = .user_code,
    .user_code_unpiped = .user_code_unpiped,
    .user = .user(),
    .message = .message(),
    .trim = FALSE
  )
}
