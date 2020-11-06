
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

  user_expr = str2expression(user_code)
  solution_expr = str2expression(solution_code)

  # if (length(solution_expr) == 0) {
  #   stop("An empty solution was used to check")
  # }

  if (identical(user_expr, solution_expr)) {
    return(NULL)
  }

  detect_mistakes(
    user = user_expr,
    solution = solution_expr,
    env = learnr::duplicate_env(envir_prep)
  )
}
