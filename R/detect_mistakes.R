detect_mistakes <- function(user,
                            solution,
                            env = rlang::env_parent(),
                            enclosing_call = NULL,
                            enclosing_arg = NULL,
                            allow_partial_matching = TRUE) {
  force(env)

  if (rlang::is_quosure(user)) {
    user <- rlang::get_expr(user)
  }
  if (rlang::is_quosure(solution)) {
    solution <- rlang::get_expr(solution)
  }

  if (is.expression(user)) {
    stopifnot(is.expression(solution))
    # need to preemptively return after each line if a result is returned
    max_len <- max(c(length(user), length(solution)))
    for (i in seq_len(max_len)) {
      if (i > length(user)) {
        return(
          missing_answer(
            this_prior_line = user[[length(user)]]
          )
        )
      }
      if (i > length(solution)) {
        return(
          extra_answer(
            this_line = user[[i]]
          )
        )
      }
      res <- detect_mistakes(
        user[[i]],
        solution[[i]],
        env = env,
        enclosing_call = enclosing_call,
        enclosing_arg = enclosing_arg,
        allow_partial_matching = allow_partial_matching
      )
      if (!is.null(res)) {
        # found a non-NULL result, return it!
        return(res)
      }
    }
    # no mistakes found above!
    return(NULL)
  }

  submitted <- user
  solution_original <- solution

  if (is.call(user)) {
    user <- unpipe_all(user) # cannot standardise yet without risking error
    submitted_names <- rlang::names2(user)
  }
  if (is.call(solution)) {
    solution <- call_standardise_formals(unpipe_all(solution), env = env)
  }

  # If the code contains a bare value, then the user and solution value
  # should be identical.
  # BUT WHAT IF ONE IS A CALL THAT EVALUATES TO THE VALUE OF THE OTHER?
  wrong_value <- detect_wrong_value(
    user, solution, submitted, enclosing_arg, enclosing_call
  )
  if (!is.null(wrong_value)) {
    return(wrong_value)
  }
  
  # We can assume anything below here is a call

  # Dividing cases into groups based on the relative lengths of the user's code
  # and the solution code produces unitelligible messages as in issue #84. To
  # produce more transparent messages that accord with how a user thinks of calls,
  # check these things in this order:

  # 2. Check that the user and the solution use the same call
  # SHOULD WE HAVE A TARGETED WRONG CALL FUNCTION?
  wrong_call <- detect_wrong_call(user, solution, enclosing_arg, enclosing_call)
  if (!is.null(wrong_call)) {
    return(wrong_call)
  }

  # 3. Check that the user code is not malformed and can be safely passed to
  # call_standardise_formals(), which uses match.call(). Malformed code may
  # contain an unused argument, multiple arguments whose names partially match
  # the same formal, duplicate argument names, or an argument whose name
  # partially matches more than one formal.
  name_problems <- detect_name_problems(
    user, solution, enclosing_arg, enclosing_call, allow_partial_matching
  )
  if (!is.null(name_problems)) {
    return(name_problems)
  }

  # 5. Check that every named argument in the solution appears in the user code.
  #    The outcome of this order is that when a user writes na = TRUE, gradethis
  #    will tell them that it expected an na.rm argument, not that na is a surplus
  #    argument.

  explicit_user <- suppressWarnings(call_standardise_formals(
    unpipe_all(submitted),
    env = env,
    include_defaults = FALSE
  ))
  explicit_solution <- call_standardise_formals(
    unpipe_all(solution_original),
    env = env,
    include_defaults = FALSE
  )
  explicit_user_names <- real_names(explicit_user)
  explicit_solution_names <-  real_names(explicit_solution)
  missing_args <- explicit_solution_names[!(explicit_solution_names %in% explicit_user_names)]

  if (length(missing_args) > 0) {
    missing_name <- missing_args[1]
    return(
      missing_argument(
        this_call = explicit_solution,
        that_name = missing_name,
        enclosing_call = enclosing_call,
        enclosing_arg = enclosing_arg
      )
    )
  }

  # It is now safe to call call_standardise_formals on student code
  user <- suppressWarnings(call_standardise_formals(user, env = env))
  user_names <- real_names(user)
  solution_names <-  real_names(solution) # original solution_names was modified above


  # 6. Check that the user code does not contain any named arguments that do not
  #    appear in the solution code. Since both calls have been standardised, these
  #    named arguments can only be being passed to ... and we should not match by
  #    position a named argument that is passed to ... with an unamed argument
  #    passed to ...

  unmatched_user_names <- user_names[!(user_names %in% solution_names)]
  if (length(unmatched_user_names) > 0) {
    surplus_name <- unmatched_user_names[1]
    return(
      surplus_argument(
        this_call = user,
        this = user[[surplus_name]],
        this_name = surplus_name,
        enclosing_call = enclosing_call,
        enclosing_arg = enclosing_arg
      )
    )
  }

  # 7. Check that every named argument in the solution matches every
  #    correspondingly named argument in the user code. We know each
  #    has a match because of Step 5.
  user_args <- as.list(user)[-1]         # remove the call
  solution_args <- as.list(solution)[-1] # remove the call
  user_named_args_ignore_list <- c()

  for (name in solution_names) {
    if (!identical(user[[name]], solution[[name]])) {
      arg_name <- ifelse(name %in% submitted_names, name, "")
      # recover the user submission as provided by only unpiping one level
      user_submitted <- call_standardise_formals(unpipe(submitted), env = env)
      res <- detect_mistakes(
        user = user_submitted[[name]],
        solution = solution[[name]],
        env = env,
        # If too verbose, use user[1]
        enclosing_call = submitted,
        # avoid naming first arguments in messages
        enclosing_arg = arg_name,
        allow_partial_matching = allow_partial_matching
      )
      if(!is.null(res)) return(res)
    }

    # Make these arguments invisible to further checks
    user_named_args_ignore_list <- c(user_named_args_ignore_list, name)
    user_args[[name]] <- NULL
    solution_args[[name]] <- NULL
  }


  # 8. Extract the remaining arguments from the user code and the solution code.
  #    Pair them in the order that they occur, checking that each pair matches.
  #    Check pairs in sequence and address unmatched arguments when you get to
  #    them.
  user_len <- length(user_args)
  solution_len <- length(solution_args)

  n <- max(user_len, solution_len)

  for (i in seq_len(n)) {

    # if solution argument is unmatched due to no remaining user arguments
    if (i > user_len) {
      name <- rlang::names2(solution_args[i])

      # if the missing argument is unnamed, pass the value
      if (is.null(name) || name == "") {
        name <- solution_args[[i]]
      }

      return(
        missing_argument(
          this_call = solution,
          that_name = name,
          enclosing_call = enclosing_call,
          enclosing_arg = enclosing_arg
        )
      )

    # if user argument is unmatched due to no remaining solution arguments
    } else if (i > solution_len) {
      arg_name <- rlang::names2(user_args[i])
      if (!(arg_name %in% submitted_names)) arg_name <- ""
      return(
        surplus_argument(
          this_call = user,
          this = user_args[[i]],
          this_name = arg_name,
          enclosing_call = enclosing_call,
          enclosing_arg = enclosing_arg
        )
      )

    # The user argument has a matching solution argument, are they identical?
    } else if (!identical(user_args[[i]], solution_args[[i]])) {
      name <- rlang::names2(user_args[i])
      if (!(name %in% submitted_names)) name <- ""
      
      # find user arg as submitted
      user_args_submitted <- as.list(call_standardise_formals(unpipe(submitted), env = env))
      user_args_ignore <- which(names(user_args_submitted) %in% user_named_args_ignore_list)
      user_args_submitted <- user_args_submitted[-c(1, user_args_ignore)]
      
      res <- detect_mistakes(
        # unpipe only one level to detect mistakes in the argument as submitted
        user = user_args_submitted[[i]],
        solution = solution_args[[i]],
        env = env,
        # If too verbose, use user[1]
        enclosing_call = submitted,
        enclosing_arg = name,
        allow_partial_matching = allow_partial_matching
      )
      if(!is.null(res)) return(res)
    }
  }

  # No missmatch found
  return(NULL)
}

real_names <- function(x) {
  x_names <- rlang::names2(x)
  x_names[x_names != ""]
}
