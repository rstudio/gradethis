detect_mistakes <- function(user, 
                            solution, 
                            env = rlang::env_parent()) {
  force(env)

  if (is.call(user)) {
    user <- unpipe_all(user) # cannot standardise yet without risking error
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
          this = deparse_to_string(user),
          that = solution
        )
      )
    }
  }
  # We can assume anything below here is a call

  user_names <- rlang::names2(user)
  solution_names <- rlang::names2(solution)
  
  # Dividing cases into groups based on the relative lengths of the user's code
  # and the solution code produces unitelligible messages as in issue #84. To
  # produce more transparent messages that accord with how a user thinks of calls,
  # check these things in this order:
  
  # 2. Check that the user and the solution use the same call
  # SHOULD WE HAVE A TARGETED WRONG CALL FUNCTION?
  if (!identical(user[[1]], solution[[1]])) {
    return(
      wrong_value(
        this = deparse_to_string(user),
        that = prep(solution),
        this_name = user_names[1],
        that_name = solution_names[1]
      )
    )
  }
  
  # 3. Check that the user code is not malformed and can be safely passed to
  # call_standardise_formals(), which uses match.call(). Malformed code may
  # contain an unused argument, may contain multiple arguments whose names
  # partially match the same formal, or an argument whose name partially matches
  # more than one formal.
  user_names <- user_names[user_names != ""]
  user_args <- as.list(user)[user_names]
  
  solution_names <- solution_names[solution_names != ""]
  solution_args <- as.list(solution)[solution_names]
  
  ## Remove exact matches from further scrutiny
  for (name in user_names) {
    if (name %in% solution_names) {
      user_args[[name]] <- NULL
      solution_args[[name]] <- NULL
    }
  }
  
  ## Check remaining arguments for partial matches
  user_names <- rlang::names2(user_args)
  
  if (length(user_names) > 0) {
    solution_names <- rlang::names2(solution_args)

    ## How many formals does each user arg partially match?
    partial_matches_per_arg <- function(user_name) {
      sum(startsWith(solution_names, user_name))
    }
    
    matches <- vapply(user_names, partial_matches_per_arg, 1)
    offenders <- matches[matches > 1]
    unused <- matches[matches == 0]
    
    # names that match multiple arguments are a syntax error
    if (length(offenders) > 0) {
      # RETURN MESSAGE with offenders[1]
      return(NULL)
    }
    
    # Unmatched named arguments are surplus
    if (length(unused) > 0) {
      surplus_name <- names(unused[1])
      return(
        surplus_argument(
          this_call = user,
          this = user[[surplus_name]],
          this_name = surplus_name
        )
      )
    }
    
    ## Do any solution names partially match multiple user names?
    partial_matches_per_formal <- function(solution_name) {
      sum(startsWith(solution_name, user_names))
    }
    
    matches <- vapply(solution_names, partial_matches_per_formal, 1)
    offenders <- matches[matches > 1]
    
    if (length(offenders) > 0) {
      # RETURN MESSAGE with offenders[1]
      return(NULL)
    }
  }
  
  user <- call_standardise_formals(user, env = env)
  user_names <- rlang::names2(user)
  solution_names <- rlang::names2(solution)
  
  # 5. Check that every named argument in the solution appears in the user code.
  #    The outcome of this order is that when a user writes na = TRUE, gradethis
  #    will tell them that it expected an na.rm argument, not that na is a surplus
  #    argument.
  actual_solution_names <-  solution_names[solution_names != ""]
  missing_args <- actual_solution_names[!(actual_solution_names %in% user_names)]
  if (length(missing_args)) {
    missing_name <- missing_args[1]
    return(
      missing_argument(
        this_call = solution,
        that = solution[[missing_name]],
        that_name = missing_name
      )
    )
  }
  
  # 6. Check that the user code does not contain any named arguments that do not
  #    appear in the solution code. Since both calls have been standardised, these
  #    named arguments can only be being passed to ... and we should not match by
  #    position a named argument that is passed to ... with an unamed argument
  #    passed to ...
  unmatched_user_names <- user_names[!(user_names %in% solution_names)] 
  unmatched_user_names <- unmatched_user_names[unmatched_user_names != ""]
  if (length(unmatched_user_names)) {
    surplus_name <- unmatched_user_names[1]
    return(
      surplus_argument(
        this_call = user,
        this = user[[surplus_name]],
        this_name = surplus_name
      )
    )
  }
  
  # 7. Check that every named argument in the solution matches every
  #    correspondingly named argument in the user code.
  for (name in actual_solution_names) {
    if (!identical(user[[name]], solution[[name]])) {
      return(
        detect_mistakes(user[[name]], solution[[name]], env = env)
      )
    }
  }
  
  
  # 8. Extract the remaining arguments from the user code and the solution code.
  #    Pair them in the order that they occur, checking that each pair matches.
  #    Check pairs in sequence and address unmatched arguments when you get to
  #    them.
  user_args <- user[-1]         # remove the call
  solution_args <- solution[-1] # remove the call
  
  for (name in actual_solution_names) {
    user_args[[name]] <- NULL
    solution_args[[name]] <- NULL
  }
  
  user_len <- length(user_args)
  solution_len <- length(solution_args)

  n <- max(user_len, solution_len)
  
  for (i in seq_len(n)) {
    
    # if solution argument is unmatched due to no remaining user arguments
    if (i > user_len) {
      return(
        missing_argument(
          this_call = solution,
          that = solution_args[[i]],
          that_name = names(solution_args[i])
        )
      )
      
    # if user argument is unmatched due to no remaining solution arguments
    } else if (i > solution_len) {
      return(
        surplus_argument(
          this_call = user,
          this = user_args[[i]],
          this_name = names(user_args[i])
        )
      )
      
    # The user argument has a matching solution argument, are they identical?
    } else {
      if (!identical(user_args[[i]], solution_args[[i]])) {
        return(
          detect_mistakes(user_args[[i]], solution_args[[i]], env = env)
        )
      }
    }
  }

  # No missmatch found
  return(NULL)
}


real_name <- function(name) {
 !is.null(name) && name != ""
}

names_match <- function(user, solution, i) {

  if (is.null(names(user))) return(TRUE)
  user_names <- names(user)

  if (is.null(names(solution))) return(TRUE)
  solution_names <- names(solution)

  if (real_name(user_names[i])) {
    if (real_name(solution_names[i])) {
      if (user_names[i] != solution_names[i]) {
        return(FALSE)
      }
    } else if (user_names[i] %in% solution_names) {
      return(FALSE)
    }
  } else if (real_name(solution_names[i]) &&
             solution_names[i] %in% user_names) {
    return(FALSE)
  }
  TRUE
}

prep_snippet <- function(code, i, .solution = FALSE) {

  # errors that involve an infix operator make more
  # sense if the explanation refers to the operator
  if (i == 2 && is_infix(code[[1]]) && length(code[[1]]) == 1) {
    paste(deparse_to_string(code[[1]][[1]]), deparse_to_string(code[[2]]))

    # Return the internal arguments of user code, but
    # not solution code (that could give away too much)
  } else if (is.call(code[[i]])) {
    ifelse(.solution, code[[i]][1], renest(code[i:length(code)]))
  } else {
    code[[i]]
  }

}
