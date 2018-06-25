detect_mistakes <- function(user,
                            solution) {
  
  # code should be checked in the opposite order 
  # of evaluation (e.g. from the outside in for 
  # nested notation), whether or not the student 
  # (and/or teacher) used a pipe
  user <- rev(order_calls(unpipe_all(user)))
  solution <- rev(order_calls(unpipe_all(solution)))
  
  max_length <- max(length(user), length(solution))
  
  for (i in seq_len(max_length)) {
    
    # Did the user miss something?
    if (i > length(user)) 
      return(missing_argument(this = user[[i-1]], 
                              that = solution[[i]], 
                              name = names(solution[i])))
    
    # Did the user write too much?
    if (i > length(solution)) 
      return(exceeded(user, i)) # I did not expect you to provide ___ as an argument to ___
                                # I did not expect you to provide an argument named ___ to ___
    
    # Does the user code not match the solution code?
    if (user[[i]] != solution[[i]])
      return(isolate_mismatch(user, solution, i))
  }
  NULL
}
    
isolate_mismatch <- function(user, solution, i) {
  
  # We've honed in on the error when we can narrow 
  # it down to a single incorrect user element 
  # matched to a single correct solution element
  if (length(user[[i]]) == 1 &&
      length(solution[[i]]) == 1) {
    return(decipher_mismatch(user, solution, i))
    
    # If we cannot do this, we are working with two 
    # multipart calls and we need to identify which 
    # elements of the calls do not align (here we 
    # rely heavily on the fact that both calls have 
    # been previously standardized)
  } else {
    user <- user[[i]]
    solution <- solution[[i]]
    
    # First check that the calls match. 
    if (user[[1]] != solution[[1]]) {
      return(wrong_value(user[[1]], solution[[1]], i))
      
      # Then inspect the arguments.
    } else {
      for (j in seq_along(user)) {
        if (j == 1) next
        
        # Did the user leave out an argument?
        if (j > length(user)) 
          return(missed_argument(this_call = user[[1]],
                                 that_name = names(solution[[j]])))
        
        # Did the user include an extra argument?
        if (j > length(solution)) 
          return(surplus_argument(this_call = user[[1]],
                                  this = user[[j]],
                                  this_name = names(user[[j]])))
        
        # Do two arguments conflict? They may themselves 
        # contain an expression that we should drill into.
        if (user[[j]] != solution[[j]]) 
          return(detect_mistakes(user, solution))
      }
    }
  }
  stop("Mismatch detected, but not spotted.")
}

# classify the extra element(s)
exceeded <- function(user, i) {
  extra <- user[[i]]
  name <- names(user[i])
  
  if (!is.null(name) && 
      name != "" && 
      length(user) > i + 1 &&
      is.call(user[[i + 1]])) {
    surplus_argument(this_call = user[[i + 1]][i], 
                     this_name = name, 
                     this = extra)
  } else if (is.call(extra)) {
    keep <- renest(user[seq_len(i-1)])
    surplus_call(extra, keep)
  } else {
    surplus_value(extra)
  }
}


# classify mismatched first elements
first_mismatched <- function(user, solution) {
  
  # Screen for missing argument
  if (is.call(user[[1]]) && 
      length(solution) > 1 &&
      is.call(solution[[2]]) &&
      user[[1]][[1]] == solution[[2]][[1]]) {
    missing_argument(this_call = user[1],
                     that_name = names(solution[1]))
    
  # Screen for surplus argument 
  # (case where the solution contains no arguments)
  } else if (is.call(solution[[1]]) && 
             length(user) > 1 &&
             is.call(user[[2]]) &&
             solution[[1]][[1]] == user[[2]][[1]]) {
    surplus_argument(this_call = solution[1],
                     this_name = names(user[1]), 
                     this = user[[1]])
    
  # Screen for surplus argument 
  # (case where the solution contains other arguments)    
  } else if (length(solution) > 1 &&
             is.call(solution[[2]]) && 
             length(user) > 1 &&
             is.call(user[[2]]) &&
             solution[[2]][[1]] == user[[2]][[1]]){
    call <- paste0(user[[2]][[1]], "()")
    surplus_argument(this_call = call,
                     this_name = names(user[1]), 
                     this = user[[1]])
    
  # Screen for the involvement of an 
  # infix operator in the user's error
  # (case where user incorectly uses an infix)
  } else if (length(user) == 2 && 
             is_infix(user[[2]]) && 
             is.call(solution[[1]])) {
      incorrect <- paste(deparse(user[[2]][[1]]), deparse(user[[1]]))
      wrong_value(this = incorrect,
                  that = solution[1])
      
  # (case where user incorectly does not use an infix)    
  } else {
    mismatched(user, solution, 1)
  }
}

