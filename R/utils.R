deparse_to_string <- function(x, width.cutoff = 500L, ...) {
  paste0(deparse(x, width.cutoff = width.cutoff, ...), collapse = "\n")
}

env_rls <- function(env) {
  if (identical(env, rlang::global_env()) || rlang::is_namespace(env)) {
    rlang::env_print(env)
    return(invisible(env))
  }
  purrr::walk(
    c(list(env), rlang::env_parents(env)),
    rlang::env_print
  )
}

r_format_code <- function(code, name = "solution") {
  tryCatch({
    x <- lapply(rlang::parse_exprs(code), rlang::expr_text)
    paste(unlist(x), collapse = "\n")
  }, error = function(err) {
    msg <- glue::glue("Unable to parse {name} code")
    grade_grading_problem(message = msg, error = err)
    rlang::abort(msg, parent = err)
  })
}
