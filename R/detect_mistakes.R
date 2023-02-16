detect_mistakes <- function(
  user,
  solution,
  env = rlang::env_parent(),
  enclosing_call = NULL,
  enclosing_arg = NULL,
  allow_partial_matching = TRUE
) {
  force(env)

  if (rlang::is_quosure(user)) {
    user <- rlang::get_expr(user)
  }
  if (rlang::is_quosure(solution)) {
    solution <- rlang::get_expr(solution)
  }

  if (is.expression(user)) {
    # An expression is made up of one or more calls.
    # This function breaks up the expression into each of its component calls
    # and runs `detect_mistakes()` on them recursively.
    return(
      detect_mistakes_expression(
        user, solution, env, enclosing_call, enclosing_arg, allow_partial_matching
      )
    )
  }

  submitted <- user
  solution_original <- solution

  if (is.call(user)) {
    user <- unpipe_all(user) # cannot standardize yet without risking error
    submitted_names <- rlang::names2(user)
  }
  if (is.call(solution)) {
    solution <- call_standardise_formals(unpipe_all(solution), env = env)
  }

  # If the code contains a bare value, then the user and solution value
  # should be identical.
  # BUT WHAT IF ONE IS A CALL THAT EVALUATES TO THE VALUE OF THE OTHER?
  return_if_not_null(
    detect_wrong_value(user, solution, submitted, enclosing_arg, enclosing_call)
  )
  # We can assume anything below here is a call

  # Dividing cases into groups based on the relative lengths of the user's code
  # and the solution code produces unintelligible messages as in issue #84. To
  # produce more transparent messages that accord with how a user thinks of calls,
  # check these things in this order:

  # 2. Check that the user and the solution use the same call
  return_if_not_null(
    detect_wrong_call(user, solution, enclosing_arg, enclosing_call)
  )

  # 3. Check that the user code is not malformed and can be safely passed to
  # call_standardise_formals(), which uses match.call(). Malformed code may
  # contain an unused argument, multiple arguments whose names partially match
  # the same formal, duplicate argument names, or an argument whose name
  # partially matches more than one formal.
  return_if_not_null(
    detect_name_problems(
      user, solution, enclosing_arg, enclosing_call, allow_partial_matching
    )
  )

  # 5. Check that every named argument in the solution appears in the user code.
  #    The outcome of this order is that when a user writes na = TRUE, gradethis
  #    will tell them that it expected an na.rm argument, not that na is a surplus
  #    argument.
  return_if_not_null(
    detect_missing_argument(
      submitted, solution_original, env, enclosing_call, enclosing_arg
    )
  )

  # It is now safe to call call_standardise_formals on student code
  user <- suppressWarnings(call_standardise_formals(user, env = env))
  user_names <- real_names(user)
  solution_names <- real_names(solution) # original solution_names was modified above


  # 6. Check that the user code does not contain any named arguments that do not
  #    appear in the solution code. Since both calls have been standardized, these
  #    named arguments can only be being passed to ... and we should not match by
  #    position a named argument that is passed to ... with an unnamed argument
  #    passed to ...
  return_if_not_null(
    detect_surplus_dots_argument(
      user, user_names, solution_names, enclosing_call, enclosing_arg
    )
  )

  # 7. Check that every named argument in the solution matches every
  #    correspondingly named argument in the user code. We know each
  #    has a match because of Step 5.
  # 8. Extract the remaining arguments from the user code and the solution code.
  #    Pair them in the order that they occur, checking that each pair matches.
  #    Check pairs in sequence and address unmatched arguments when you get to
  #    them.
  return_if_not_null(
    detect_wrong_arguments(
      user,
      solution,
      solution_names,
      submitted,
      submitted_names,
      env,
      enclosing_call,
      enclosing_arg,
      allow_partial_matching
    )
  )

  # No mismatch found
  return(NULL)
}

detect_mistakes_expression <- function(
  user, solution, env, enclosing_call, enclosing_arg, allow_partial_matching
) {
  stopifnot(is.expression(solution))

  # need to preemptively return after each line if a result is returned
  max_len <- max(c(length(user), length(solution)))

  for (i in seq_len(max_len)) {
    if (i > length(user)) {
      return(message_missing_answer(this_prior_line = user[[length(user)]]))
    }

    if (i > length(solution)) {
      return(message_extra_answer(this_line = user[[i]]))
    }

    return_if_not_null(
      detect_mistakes(
        user[[i]],
        solution[[i]],
        env = env,
        enclosing_call = enclosing_call,
        enclosing_arg = enclosing_arg,
        allow_partial_matching = allow_partial_matching
      )
    )
  }

  # no mistakes found above!
  return(NULL)
}

real_names <- function(x) {
  x_names <- rlang::names2(x)
  x_names[x_names != ""]
}
