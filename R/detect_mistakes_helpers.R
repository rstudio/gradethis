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
  if (!is.null(duplicate_names)) {
    return(duplicate_names)
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
    too_many_matches <- detect_too_many_matches(
      user, remaining_solution_names, remaining_user_names,
      enclosing_call, enclosing_arg
    )
    if (!is.null(too_many_matches)) {
      return(too_many_matches)
    }

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
    if (!is.null(bad_argument_names)) {
      return(bad_argument_names)
    }

    # Unmatched named arguments are surplus
    surplus_argument <- detect_surplus_argument(
      user, unused, enclosing_call, enclosing_arg
    )
    if (!is.null(surplus_argument)) {
      return(surplus_argument)
    }


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
        if (!is.null(pmatches_argument_name)) {
          return(pmatches_argument_name)
        }
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
  if (!is.null(unnamed_surplus_argument)) {
    return(unnamed_surplus_argument)
  }

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