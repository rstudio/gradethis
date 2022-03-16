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

env_insert_parent <- function(child, parent_new) {
  if (!rlang::is_environment(child) || !rlang::is_environment(parent_new)) {
    return(invisible(child))
  }
  if (identical(child, parent_new)) {
    return(invisible(child))
  }
  if (identical(child, globalenv())) {
    rlang::abort("`child` is the global env, cannot insert parent above it")
  }

  # if parent_new is already in the env chain for child: no-op
  child_stack <- rlang::env_parents(child)
  parent_in_child_stack <- any(
    vapply(child_stack, function(env) identical(env, parent_new), logical(1))
  )
  if (parent_in_child_stack) {
    return(invisible(child))
  }

  # 1. Starting with child and parent_new
  #
  #            parent_new
  #
  # +------------+      +---------+
  # | parent_old +----->|  child  |
  # +------------+      +---------+
  #
  # 2. Make parent_old the parent env of parent_new
  #
  #          +------------+
  #       +->| parent_new |
  #       |  +------------+
  #       |
  # +-----+------+      +---------+
  # | parent_old |      |  child  |
  # +------------+      +---------+
  #
  # 3. Re-attach child to parent_new
  #
  #          +------------+
  #       +->| parent_new +--+
  #       |  +------------+  |
  #       |                  v
  # +-----+------+      +---------+
  # | parent_old |      |  child  |
  # +------------+      +---------+
  #
  # 4. Watch out! If parent_new or child are the global env...

  if (identical(parent_new, globalenv())) {
    # parent_new is global env but not in the env stack of child
    # so attach highest parent of child to globalenv
    child_top <- rlang::env_tail(child)
    if (!identical(rlang::env_parent(child_top), globalenv())) {
      rlang::env_poke_parent(child_top, parent_new)
    }
    return(invisible(child))
  }

  parent_old <- rlang::env_parent(child)
  rlang::env_poke_parent(parent_new, parent_old)
  rlang::env_poke_parent(child, parent_new)
  invisible(child)
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
