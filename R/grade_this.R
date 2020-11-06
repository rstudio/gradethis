
# #' @export
# last_value <- function(
#   env = parent.frame()
# ) {
#   if (!exists(".result", envir = env, inherits = TRUE)) {
#     stop("Could not find `.result`. Be sure to only use `last_value()` inside `grade_this()`")
#   }
#   get(".result", envir = env, inherits = TRUE)
# }

#' @export
grade_this <- function(expr) {
  # sometimes expr is a quosure already, so squash all quosures to a single expression
  express <- rlang::get_expr(rlang::enquo(expr))

  function(checking_env) {
    eval_gradethis({
      rlang::eval_tidy(
        express,
        env = checking_env
      )
    })
  }
}

#' @export
grade_this_code <- function(
  correct = getOption("gradethis.code.correct", getOption("gradethis.pass", "Correct!")),
  incorrect = getOption("gradethis.code.incorrect", getOption("gradethis.fail", "Incorrect"))
) {

  # MUST wrap calling function to be able to shim in `.correct`/`.incorrect`
  function(checking_env) {
    checking_env[[".__correct"]] <- correct
    checking_env[[".__incorrect"]] <- incorrect

    grade_this({
      # create variable `.message` for glue to find
      .message <- code_feedback()
      # call `pass`/`fail` inside `grade_this` to have access to `checking_env`
      if (is.null(.message)) {
        # need to use `get()` to avoid using `utils::globalVariables`
        pass(get(".__correct"))
      } else {
        fail(get(".__incorrect"))
      }
    })(checking_env)
  }
}

get_from <- function(name, envir, default = "") {
  if (!exists(name, envir = envir, inherits = TRUE)) {
    return(default)
  }
  get(name, envir = envir, inherits = TRUE)
}
#' @export
code_feedback <- function(
  user_code = get_from(".user_code", envir = env, default = ""),
  solution_code = get_from(".solution_code", envir = env, default = ""),
  envir_prep = get_from(".envir_prep", envir = env, default = new.env(parent = globalenv())),
  env = parent.frame()
) {

  chkm8_single_character(user_code, null.ok = FALSE)
  chkm8_single_character(solution_code, null.ok = FALSE)
  checkmate::assert_environment(envir_prep, null.ok = FALSE, .var.name = "envir_prep")

  user_expr = str2expression(user_code)
  solution_expr = str2expression(solution_code)

  if (identical(user_expr, solution_expr)) {
    # identical! return early
    return(NULL)
  }

  detect_mistakes(
    user = user_expr,
    solution = solution_expr,
    env = new.env(parent = envir_prep)
  )
}
