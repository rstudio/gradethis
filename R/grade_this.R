

# #' @export
# last_result <- function(
#   env = parent.frame()
# ) {
#   if (!exists(".result", envir = env, inherits = TRUE)) {
#     stop("Could not find `.result`. Be sure to only use `last_result()` inside `grade_this()`")
#   }
#   get(".result", envir = env, inherits = TRUE)
# }

#' @export
grade_this <- function(expr) {
  expr_quo <- rlang::enquo(expr)
  function(check_obj) {
    eval_gradethis_expr({
      rlang::eval_tidy(
        rlang::get_expr(expr_quo),
        env = check_obj
      )
    })
  }
}


# grade_this_code <- function(correct = "{random_praise()}", incorrect = "{random_encouragement()}") {
#   grade_this({
#     res <- code_feedback(user = .user_code, solution = .solution_code)
#     if (isTRUE(res)) {
#       pass(correct)
#     } else {
#       fail(incorrect)
#     }
#   })
# }
