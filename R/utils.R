deparse_to_string <- function(x, width.cutoff = 500L, ...) { # nolint: object_name
  paste0(deparse(x, width.cutoff = width.cutoff, ...), collapse = "\n")
}

local_env_insert_parent <- function(
  child,
  parent_new,
  restore_env = parent.frame()
) {
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

  # Put parent_new back where we found it when exiting `restore_env`
  original_parent_of_parent_new <- rlang::env_parent(parent_new)
  withr::defer(
    rlang::env_poke_parent(parent_new, original_parent_of_parent_new),
    envir = restore_env
  )

  parent_old <- rlang::env_parent(child)
  rlang::env_poke_parent(parent_new, parent_old)
  rlang::env_poke_parent(child, parent_new)
  invisible(child)
}


# nocov start
env_rls <- function(env, show_contents = TRUE) {
  env_start <- rlang::as_label(rlang::enquo(env))

  if (identical(env, rlang::global_env()) || rlang::is_namespace(env)) {
    rlang::env_print(env)
    return(invisible(env))
  }

  stack <- c(list(env), rlang::env_parents(env))

  if (show_contents) {
    purrr::walk(stack, rlang::env_print)
  } else {
    env_names <- purrr::map_chr(stack, rlang::env_label)
    env_names <- paste0("<env ", env_names, ">")
    env_names[1] <- paste0(env_start, ": ", env_names[1])
    names(env_names) <- c("", rep("*", length(env_names) - 1))
    msg <- rlang::cnd_message(rlang::catch_cnd(rlang::inform(env_names)))
    cat("\n", msg, "\n", sep = "")
  }
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
# nocov end
