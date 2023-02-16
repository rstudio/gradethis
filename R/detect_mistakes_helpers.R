detect_wrong_value <- function(
  user, solution, submitted, enclosing_arg, enclosing_call
) {
  if (is.call(user) && is.call(solution)) {
    return()
  }

  if (identical(user, solution)) {
    return()
  }

  if (detect_mismatched_function_arguments(user, solution)) {
    submitted <- as.pairlist(user[setdiff(names(user), names(solution))])
    solution <- as.pairlist(solution[setdiff(names(solution), names(user))])
  }

  message_wrong_value(
    submitted = submitted,
    solution = solution,
    submitted_name = enclosing_arg,
    enclosing_call = enclosing_call
  )
}

detect_mismatched_function_arguments <- function(user, solution) { # nolint: object_length
  is.pairlist(user) &&
    is.pairlist(solution) &&
    length(user) != length(solution)
}

detect_wrong_call <- function(user, solution, enclosing_arg, enclosing_call) {
  if (identical(as.list(user)[[1]], as.list(solution)[[1]])) {
    return()
  }

  message_wrong_call(
    submitted = user,
    solution = solution,
    submitted_name = enclosing_arg,
    enclosing_call = enclosing_call
  )
}

detect_name_problems <- function(
  user, solution, enclosing_arg, enclosing_call, allow_partial_matching
) {
  user_args <- as.list(user)
  user_names <- real_names(user)

  solution_args <- as.list(solution)
  solution_names <- real_names(solution)

  ## If the user duplicates an argument name, ensure that the solution does as
  ## well. This should rarely happen, but might with map() for example.
  duplicate_names <- detect_duplicate_names(
    user, user_names, solution_names, enclosing_arg, enclosing_call
  )
  return_if_not_null(duplicate_names)

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
    too_many_matches <- detect_too_many_matches(
      user, remaining_solution_names, remaining_user_names,
      enclosing_call, enclosing_arg
    )
    return_if_not_null(too_many_matches)

    ## How many formals does each remaining user arg partially match?
    pmatches_per_arg <- function(user_name) {
      sum(startsWith(remaining_solution_names, user_name))
    }

    matches <- vapply(remaining_user_names, pmatches_per_arg, 1)
    unused <- matches[matches == 0]
    well_matched <- matches[matches == 1]

    # names that match multiple arguments are a syntax error
    bad_argument_names <- detect_bad_argument_names(
      user, matches, enclosing_call, enclosing_arg
    )
    return_if_not_null(bad_argument_names)

    # Unmatched named arguments are surplus
    surplus_argument <- detect_surplus_argument(
      user, unused, enclosing_call, enclosing_arg
    )
    return_if_not_null(surplus_argument)


    if (length(well_matched > 0)) {
      matched_user_names <- rlang::names2(well_matched)

      if (!allow_partial_matching) {
        pmatches_argument_name <- detect_pmatches_argument_name(
          user,
          remaining_user_names,
          remaining_solution_names,
          matched_user_names,
          matched_solution_names,
          enclosing_call,
          enclosing_arg
        )
        return_if_not_null(pmatches_argument_name)
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

  unnamed_surplus_argument <- detect_unnamed_surplus_argument(
    user, user_args, solution_args, enclosing_call, enclosing_arg
  )
  return_if_not_null(unnamed_surplus_argument)

  invisible()
}

detect_duplicate_names <- function(
  user, user_names, solution_names, enclosing_call, enclosing_arg
) {
  user_arg_ns <- table(user_names)
  solution_arg_ns <- table(solution_names)
  if (any(user_arg_ns > 1)) {
    duplicates <- names(user_arg_ns[user_arg_ns > 1])
    for (name in duplicates) {
      if (!identical(user_arg_ns[name], solution_arg_ns[name])) {
        return(
          message_duplicate_name(
            submitted_call = user,
            submitted_name = name,
            enclosing_call = enclosing_call,
            enclosing_arg = enclosing_arg
          )
        )
      }
    }
  }
}

detect_too_many_matches <- function(
  user,
  remaining_solution_names,
  remaining_user_names,
  enclosing_call,
  enclosing_arg
) {
  ## Do any non-matched solution names partially match multiple user names?
  pmatches_per_formal <- function(solution_name) {
    sum(startsWith(solution_name, remaining_user_names))
  }

  matches <- vapply(remaining_solution_names, pmatches_per_formal, 1)
  offenders <- matches[matches > 1]

  if (length(offenders) > 0) {
    overmatched_name <- rlang::names2(offenders[1])
    return(
      message_too_many_matches(
        submitted_call = user,
        solution_name = overmatched_name,
        enclosing_call = enclosing_call,
        enclosing_arg = enclosing_arg
      )
    )
  }
}

detect_bad_argument_names <- function(
  user, matches, enclosing_call, enclosing_arg
) {
  offenders <- matches[matches > 1]

  if (length(offenders) == 0) {
    return()
  }

  bad_name <- rlang::names2(offenders[1])
  message_bad_argument_name(
    submitted_call = user,
    submitted = user[[bad_name]],
    submitted_name = bad_name,
    enclosing_call = enclosing_call,
    enclosing_arg = enclosing_arg
  )
}

detect_surplus_argument <- function(
  user, unused, enclosing_call, enclosing_arg
) {
  if (length(unused) > 0) {
    surplus_name <- rlang::names2(unused[1])
    return(
      message_surplus_argument(
        submitted_call = user,
        submitted = user[[surplus_name]],
        submitted_name = surplus_name,
        enclosing_call = enclosing_call,
        enclosing_arg = enclosing_arg
      )
    )
  }
}

detect_pmatches_argument_name <- function(
  user,
  remaining_user_names,
  remaining_solution_names,
  matched_user_names,
  matched_solution_names,
  enclosing_call,
  enclosing_arg
) {
  ## where does partial matching occur ?
  where_pmatches <- function(user_name) {
    which(startsWith(remaining_solution_names, user_name))
  }

  matches <- vapply(remaining_user_names, where_pmatches, 1)
  matched_solution_name <- remaining_solution_names[matches]

  message_pmatches_argument_name(
    submitted_call = user,
    submitted = unname(as.list(user)[matched_user_names]),
    submitted_name = matched_user_names,
    solution_name = matched_solution_name,
    enclosing_call = enclosing_call,
    enclosing_arg = enclosing_arg
  )
}

detect_unnamed_surplus_argument <- function(
  user, user_args, solution_args, enclosing_call, enclosing_arg
) {
  # Check for unnamed, unused arguments
  # Any further matching will now be by position not name
  n_remaining_user <- length(user_args)
  n_remaining_solution <- length(solution_args)

  if (n_remaining_user > n_remaining_solution) {
    i <- n_remaining_solution + 1
    return(
      message_surplus_argument(
        submitted_call = user,
        submitted = user_args[[i]],
        submitted_name = rlang::names2(user_args[i]),
        enclosing_call = enclosing_call,
        enclosing_arg = enclosing_arg
      )
    )
  }
}

detect_missing_argument <- function(
  submitted, solution_original, env, enclosing_call, enclosing_arg
) {
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

  explicit_solution_names <- real_names(explicit_solution)
  missing_args <- setdiff(explicit_solution_names, explicit_user_names)

  if (length(missing_args) == 0) {
    return()
  }

  missing_name <- missing_args[1]
  message_missing_argument(
    submitted_call = explicit_solution,
    solution_name = missing_name,
    enclosing_call = enclosing_call,
    enclosing_arg = enclosing_arg
  )
}

detect_surplus_dots_argument <- function(
  user, user_names, solution_names, enclosing_call, enclosing_arg
) {
  unmatched_user_names <- setdiff(user_names, solution_names)

  if (length(unmatched_user_names) == 0) {
    return()
  }

  surplus_name <- unmatched_user_names[1]
  message_surplus_argument(
    submitted_call = user,
    submitted = user[[surplus_name]],
    submitted_name = surplus_name,
    enclosing_call = enclosing_call,
    enclosing_arg = enclosing_arg
  )
}

detect_wrong_arguments <- function(
  user,
  solution,
  solution_names,
  submitted,
  submitted_names,
  env,
  enclosing_call,
  enclosing_arg,
  allow_partial_matching
) {
  user_args <- as.list(user)[-1]         # remove the call
  solution_args <- as.list(solution)[-1] # remove the call
  user_named_args_ignore_list <- c()

  # Check that every named argument in the solution matches every
  # correspondingly named argument in the user code. We know each
  # has a match because of Step 5.
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
      return_if_not_null(res)
    }

    # Make these arguments invisible to further checks
    user_named_args_ignore_list <- c(user_named_args_ignore_list, name)
    user_args[[name]] <- NULL
    solution_args[[name]] <- NULL
  }


  # Extract the remaining arguments from the user code and the solution code.
  # Pair them in the order that they occur, checking that each pair matches.
  # Check pairs in sequence and address unmatched arguments when you get to
  # them.
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
        message_missing_argument(
          submitted_call = solution,
          solution_name = name,
          enclosing_call = enclosing_call,
          enclosing_arg = enclosing_arg
        )
      )

      # if user argument is unmatched due to no remaining solution arguments
    } else if (i > solution_len) {
      arg_name <- rlang::names2(user_args[i])
      if (!(arg_name %in% submitted_names)) arg_name <- ""
      return(
        message_surplus_argument(
          submitted_call = user,
          submitted = user_args[[i]],
          submitted_name = arg_name,
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
      return_if_not_null(res)
    }
  }
}
