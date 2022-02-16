.pipes <- c("%>%")

is_pipe <- function(x) {
  if (is.character(x)) x <- parse(text = x)[[1]]
  ifelse(is.call(x), as.character(x[[1]]) %in% .pipes, FALSE)
}

.infixes_assign <- c("<-", "<<-", "->", "->>", "=")
.infixes_comp <- c("==", "!=", ">", ">=", "<", "<=")
.infixes <- c(
  "+", "-", "*", "/", "^", "$", "[", "[[", "!", "%%", "%/%", "%in%",
  .infixes_assign,
  .infixes_comp
)

is_infix <- function(x, infix_vals = .infixes) {

  tryCatch({
    if (is.character(x)) {
      out <- str2lang(x)
    } else {
      out <- x
    }

    if (!is.call(out)) {
      return(FALSE)
    }

    any(as.character(out[[1]]) %in% infix_vals)
  }, error = function(e) {
    # x is not an infix
    FALSE
  })
}

is_infix_assign <- function(x) {
  is_infix(x, infix_vals = .infixes_assign)
}

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

is_tag_like <- function(x) {
  inherits(x, c("shiny.tag", "shiny.tag.list"))
}

is_AsIs <- function(x) {
  inherits(x, "AsIs")
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
