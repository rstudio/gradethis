detect_mistakes <- function(user, 
                            solution, 
                            env = rlang::env_parent()
                            ) {
  force(env)

  if (is.call(user)) {
    user <- unpipe_all(user)
  }
  
  if (is.call(solution)) {
    solution <- call_standardise_formals(unpipe_all(solution), env = env)
  }
  
  # if the code is not a call, it 
  # should be a value identical to the solution
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
  
  # Do user and solution call the same function?
  if (!identical(user[[1]], solution[[1]])) {
    return(
      wrong_value(
        this = deparse_to_string(user),
        that = prep(solution),
        this_name = rlang::names2(user)[1],
        that_name = rlang::names2(solution)[1]
      )
    )
  }

  # Check user arguments whose name exactly matches a name in the solution
  user_args <- as.list(user[-1])
  user_names <- rlang::names2(user_args)
  user_names <- user_names[user_names != ""]
  
  solution_args <- as.list(solution[-1])
  solution_names <- rlang::names2(solution_args)
  solution_names <- solution_names[solution_names != ""]
  
  for (name in user_names) {
    if (name %in% solution_names) {
      if (!identical(user[[name]], solution[[name]])) {
        return(
          detect_mistakes(
            user = user[[name]],
            solution = solution[[name]],
            env = env
          )
        )
      }
      
      # Make matched arguments invisible to further searches
      user_args[[name]] <- NULL
      solution_args[[name]] <- NULL
    }
  }
  
  ## Can remaining names be partially matched to names in the solution?
  user_names <- rlang::names2(user_args)
  user_names <- user_names[user_names != ""]
    
  if (length(user_names) > 0) {
    
    solution_names <- rlang::names2(solution_args)
    solution_names <- solution_names[solution_names != ""]
    
    ## Do any user names partially match multiple solution names?
    partial_matches_per_arg <- function(user_name) {
      sum(startsWith(solution_names, user_name))
    }
    
    matches <- vapply(user_names, partial_matches_per_arg, 1)
    offenders <- matches[matches > 1]
    
    if (length(offenders) > 0) {
      # RETURN MESSAGE with offenders[1]
      return(NULL)
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
    
    # Check unmatched user argument names for partial matches to solution names
    for (name in user_names) {
      match <- which(startsWith(solution_names, name))
      if (length(match) > 0) {
        solution_name <- solution_names[match]
        if (!identical(user[[name]], solution[[solution_name]])) {
          return(
            detect_mistakes(
              user = user[[name]],
              solution = solution[[solution_name]],
              env = env
            )
          )
        }
        
        # Make matched arguments invisible to further searches
        user_args[[name]] <- NULL
        solution_args[[solution_name]] <- NULL
      }
    }
    
    # If any unmatched named user arguments remain, they are surplus
    user_names <- rlang::names2(user_args)
    user_names <- user_names[user_names != ""]
  
    if (length(user_names) > 0) {
      name <- user_names[1]
      return(
        surplus_argument(
          this_call = user,
          this = user[[name]],
          this_name = name
        )
      )
    }
  }
  
  # Compare all remaining arguments by position
  n_user_remain <- length(user_args)
  n_solution_remain <- length(solution_args)
  n <- max(n_user_remain, n_solution_remain)
  
  for (i in seq_len(n)) {
    if (i > n_solution_remain) {
      return(
        surplus_argument(
          this_call = user,
          this = user_args[[i]]
        )
      )
    }
    
    if (i > n_user_remain) {
      return(
        missing_argument(
          this_call = user,
          that = solution_args[[i]],
          that_name = rlang::names2(solution_args)[i]
        )
      )
    }
    
    if (!identical(user_args[[i]], solution_args[[i]])) {
      return(
        detect_mistakes(
          user = user_args[[i]], 
          solution = solution_args[[i]], 
          env = env
        )
      )
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
