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

  if (is.call(user)) {
    user <- unpipe_all(user) # cannot standardise yet without risking error
    submitted_names <- rlang::names2(user)
  }
  if (is.call(solution)) {
    solution <- call_standardise_formals(unpipe_all(solution), env = env)
  }

  # 1. If the code contains a bare value, then the user and solution value
  # should be identical.
  # BUT WHAT IF ONE IS A CALL THAT EVALUATES TO THE VALUE OF THE OTHER?
  if (!is.call(user) || !is.call(solution)) {
    if (!identical(user, solution)) {
      return(
        wrong_value(
          this = submitted,
          that = solution,
          this_name = enclosing_arg,
          enclosing_call = enclosing_call
        )
      )
    }
  }
  # We can assume anything below here is a call

  # Dividing cases into groups based on the relative lengths of the user's code
  # and the solution code produces unitelligible messages as in issue #84. To
  # produce more transparent messages that accord with how a user thinks of calls,
  # check these things in this order:

  # 2. Check that the user and the solution use the same call
  # SHOULD WE HAVE A TARGETED WRONG CALL FUNCTION?
  if (!identical(user[[1]], solution[[1]])) {
    return(
      wrong_call(
        this = user,
        that = solution,
        this_name = enclosing_arg,
        enclosing_call = enclosing_call
      )
    )
  }

  # 3. Check that the user code is not malformed and can be safely passed to
  # call_standardise_formals(), which uses match.call(). Malformed code may
  # contain an unused argument, multiple arguments whose names partially match
  # the same formal, duplicate argument names, or an argument whose name
  # partially matches more than one formal.
  user_args <- as.list(user)
  user_names <- real_names(user)

  solution_args <- as.list(solution)
  solution_names <- real_names(solution)

  ## If the user duplicates an argument name, ensure that the solution does as
  ## well. This should rarely happen, but might with map() for example.
  user_arg_ns <- table(user_names)
  solution_arg_ns <- table(solution_names)
  if (any(user_arg_ns > 1)) {
    duplicates <- names(user_arg_ns[user_arg_ns > 1])
    for (name in duplicates) {
      if (!identical(user_arg_ns[name], solution_arg_ns[name])) {
        return(
          duplicate_name(
            this_call = user,
            this_name = name,
            enclosing_call = enclosing_call,
            enclosing_arg = enclosing_arg
          )
        )
      }
    }
  }

  ## Remove exact matches from further scrutiny
  for (name in user_names) {
    if (name %in% solution_names) {
      user_args[[name]] <- NULL
      solution_args[[name]] <- NULL

      # remove first instance of name from real solution
      # names to handle duplicated argument names
      name_index <- which(identical(solution_names, name))[1] 
      solution_names[name_index] <- ""
    }
  }

  ## Check remaining arguments for partial matches
  remaining_user_names <- real_names(user_args)
  remaining_solution_names <- real_names(solution_args)

  if (length(remaining_user_names) > 0) {

    ## Do any non-matched solution names partially match multiple user names?
    pmatches_per_formal <- function(solution_name) {
      sum(startsWith(solution_name, remaining_user_names))
    }

    matches <- vapply(remaining_solution_names, pmatches_per_formal, 1)
    offenders <- matches[matches > 1]

    if (length(offenders) > 0) {
      overmatched_name <- rlang::names2(offenders[1])
      return(
        too_many_matches(
          this_call = user,
          that_name = overmatched_name,
          enclosing_call = enclosing_call,
          enclosing_arg = enclosing_arg
        )
      )
    }

    ## How many formals does each remaining user arg partially match?
    pmatches_per_arg <- function(user_name) {
      sum(startsWith(remaining_solution_names, user_name))
    }

    matches <- vapply(remaining_user_names, pmatches_per_arg, 1)
    offenders <- matches[matches > 1]
    unused <- matches[matches == 0]
    well_matched <- matches[matches == 1]

    # names that match multiple arguments are a syntax error
    if (length(offenders) > 0) {
      bad_name <- rlang::names2(offenders[1])
      return(
        bad_argument_name(
          this_call = user,
          this = user[[bad_name]],
          this_name = bad_name,
          enclosing_call = enclosing_call,
          enclosing_arg = enclosing_arg
        )
      )
    }

    # Unmatched named arguments are surplus
    if (length(unused) > 0) {
      surplus_name <- rlang::names2(unused[1])
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

    # donc ici on accepte le partial matching
    
    if (length(well_matched > 0)){
      matched_user_names <- rlang::names2(well_matched)
      
      
      
      
    if ( !allow_partial_matching ){
    
      matches <- which(sapply(lapply(remaining_solution_names,startsWith, matched_user_names),sum)==1)
      
      matched_solution_name <- remaining_solution_names[matches]
      return(
        pmatches_argument_name(
          this_call = user,
          this = as.character(user[matched_user_names]),
          this_name = matched_user_names,
          correct_name = matched_solution_name,
          enclosing_call = enclosing_call,
          enclosing_arg = enclosing_arg
        )
      )
      
    }
    
    
    # Remove partially matched arguments from further consideration

      for (name in matched_user_names) {
        # which solution name does it match?
        match <- which(startsWith(remaining_solution_names, name))
        matched_solution_name <- remaining_solution_names[match]

        user_args[[name]] <- NULL
        solution_args[[matched_solution_name]] <- NULL
      }
    }

  }

  # Check for unnamed, unused arguments
  # Any further matching will now be by position not name
  n_remaining_user <- length(user_args)
  n_remaining_solution <- length(solution_args)
  if (n_remaining_user > n_remaining_solution) {
    i <- n_remaining_solution + 1
    return(
      surplus_argument(
        this_call = user,
        this = user[[i]],
        this_name = rlang::names2(user[i]),
        enclosing_call = enclosing_call,
        enclosing_arg = enclosing_arg
      )
    )
  }

  # It is now safe to call call_standardise_formals on student code
  user <- call_standardise_formals(user, env = env)
  user_names <- real_names(user)

  # 5. Check that every named argument in the solution appears in the user code.
  #    The outcome of this order is that when a user writes na = TRUE, gradethis
  #    will tell them that it expected an na.rm argument, not that na is a surplus
  #    argument.
  solution_names <-  real_names(solution) # original solution_names was modified above
  missing_args <- solution_names[!(solution_names %in% user_names)]
  if (length(missing_args) > 0) {
    missing_name <- missing_args[1]
    return(
      missing_argument(
        this_call = solution,
        that_name = missing_name,
        enclosing_call = enclosing_call,
        enclosing_arg = enclosing_arg
      )
    )
  }

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

  for (name in solution_names) {
    if (!identical(user[[name]], solution[[name]])) {
      arg_name <- ifelse(name %in% submitted_names, name, "")
      res <- detect_mistakes(
        user = user[[name]],
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
      res <- detect_mistakes(
        user = user_args[[i]],
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
