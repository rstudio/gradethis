#' Inform the user about how gradethis interprets piped code
#'
#' Creates a warning message when user code contains the `%>%`. When feedback
#' is automatically generated via [code_feedback()] or in [grade_this_code()],
#' this message attempts to contextualize feedback that might make more sense
#' when referenced against an un-piped version of the student's code.
#'
#' @section Options:
#'
#' - `gradethis.pipe_warning`: The default pipe warning message is set via this
#'   option.
#'
#' @section Glue Variables:
#'
#' The following variables may be used in the glue-able `message`:
#'
#' - `.user_code`: The student's original submitted code.
#'
#' - `.user_code_unpiped`: The unpiped version of the student's submitted code.
#'
#' @examples
#' # The default `pipe_warning()` message:
#' getOption("gradethis.pipe_warning")
#'
#' # Let's consider two versions of the user code
#' user_code <-  "penguins %>% pull(year) %>% min(year)"
#' user_code_unpiped <- "min(pull(penguins, year), year)"
#'
#' # A `pipe_warning()` is created when the user's code contains `%>%`
#' pipe_warning(.user_code = user_code)
#'
#' # And no message is created when the user's code in un-piped
#' pipe_warning(.user_code = user_code_unpiped)
#'
#' # Typically, this warning is only introduced when giving code feedback
#' # for an incorrect submission. Here we didn't expect `year` in `min()`.
#' submission <- mock_this_exercise(
#'   .user_code = !!user_code,
#'   .solution_code = "penguins %>% pull(year) %>% min()"
#' )
#'
#' grade_this_code()(submission)
#' @param message A glue string containing the message. The default value is set
#'   with the `gradethis.pipe_warning` option.
#' @param .user_code The user's submitted code, found in `env` if `NULL`
#'
#' @return Returns a string containing the pipe warning message, or an empty
#'   string if the `.user_code` does not contain a pipe, if the `.user_code` is
#'   also empty, or if the `message` is `NULL`.
#'
#' @export
pipe_warning <- function(
  message = getOption("gradethis.pipe_warning"),
  .user_code = NULL
) {
  if (is.null(message) || identical(trimws(message), "")) {
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
