
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
  # sometimes expr is a quosure already, so squash all quosures to a single expression
  express <- rlang::get_expr(rlang::enquo(expr))

  function(checking_env) {
    eval_gradethis_expr({
      rlang::eval_tidy(
        express,
        env = checking_env
      )
    })
  }
}

#' @export
grade_this_code <- function(
  correct = "Correct! {random_praise()}",
  incorrect = "{.message} {random_encouragement()}"
) {

  # MUST wrap calling function to be able to shim in `.correct`/`.incorrect`
  function(checking_env) {
    checking_env$.correct <- correct
    checking_env$.incorrect <- incorrect

    grade_this({
      # create variable `.message` for glue to find
      .message <- code_feedback()
      # call `pass`/`fail` inside `grade_this` to have access to `checking_env`
      if (is.null(.message)) {
        # need to use `get()` to avoid using `utils::globalVariables`
        pass(get(".correct"))
      } else {
        fail(get(".incorrect"))
      }
    })(checking_env)
  }
}


#' @export
code_feedback <- function(
  user_code = get(".user_code", envir = env, inherits = TRUE),
  solution_code = get(".solution_code", envir = env, inherits = TRUE),
  envir_prep = get(".envir_prep", envir = env, inherits = TRUE),
  env = parent.frame()
) {

  chkm8_single_character(user_code, null.ok = FALSE)
  chkm8_single_character(solution_code, null.ok = FALSE)
  checkmate::assert_environment(env, null.ok = FALSE, .var.name = "env")

  detect_mistakes(
    user = str2expression(user_code),
    solution = str2expression(solution_code),
    env = learnr::duplicate_env(envir_prep)
  )
}
