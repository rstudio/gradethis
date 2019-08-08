#' TODO document with 'tests' documentation
#' @export
#' @rdname test
#' @param message Message to report back if the test throws an error.
#' @param fn function to execute against the user solution.
#'   If the test fails, it should throw an error to display the \code{message} provided.
# test <- function(message, fn) {
#   function(x) {
#     tryCatch(
#       { # nolint
#         fn(x)
#       },
#       error = function(e) {
#         stop(message, call. = FALSE)
#       }
#     )
#   }
# }
