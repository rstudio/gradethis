detect_mistakes <- function(user,
                            solution) {
  
  # code should be checked in the order 
  # of evaluation, whether or not the 
  # student (and/or teacher) used a pipe
  user <- order_calls(unpipe_all(user))
  solution <- order_calls(unpipe_all(solution))
  
  max_length <- max(length(user), length(solution))
  
  for (i in seq_len(max_length)) {
    
    # Did the user miss something?
    if (i > length(user)) 
      return(missed(solution, i))
    
    # Did the user write too much?
    if (i > length(solution)) 
      return(exceeded(user, i))
    
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
      return(decipher_mismatch(user[[1]], solution[[1]], i))
      
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

# classify mismatched elements
decipher_mismatch <- function(user, solution, i) {
  
  # A mismatch can be the source of an error, or it can indicate 
  # an error that occurs further up the call trace (e.g. two 
  # arguments are likely to mismatch if they are intended for two 
  # different functions). This code reviews the remainder of the 
  # call trace to spot the ultimate source of the error.
  
  # Extract the call trace that led to the mismatch
  user_len <- length(user)
  solution_len <- length(solution)
  
  user_trace <- rev(user[i:user_len])
  solution_trace <- rev(solution[i:solution_len])
  
  remove_arguments <- function(x) if(is.call(x)) as.call(as.list(x[[1]]))
  user_trace <- purrr::map(user_trace, remove_arguments)
  solution_trace <- purrr::map(solution_trace, remove_arguments)
  
  # Compare items on the trace one at a time to spot the error
  max_length <- max(length(user), length(solution))
  for (k in seq_len(max_length)) {
    # Did the user miss a call?
    if (k > length(user_trace)) {
      if (user_len - k
      return(wrong_value(user[user_len - k], solution[k]))
    }
    # Did the user insert an extra call?
    # Did the user call the wrong call?
  }
  
  # The mismatched element is a call *on something*
} else if (i != 1 && is.call(user[[i]]) && is.call(solution[[i]])) {
  wrong_call(this_call = user[i], 
             this = renest(user[seq_len(i-1)]), 
             that_call = solution[i])
} else {
  wrong_value(this = user[[i]], that = solution[[i]])
}
}
        

# classify missed elements
missed <- function(solution, i){
  name <- names(solution[i - 1])
  
  if (!is.null(name) && 
      name != "" &&
      is.call(user[[i - 1]]) &&
      is.call(solution[[i]]) &&
      solution[[i]][[1]] == user[[i - 1]][[1]]) {
    missing_argument(this_call = user[[i - 1]][1], 
                     that_name = name)
  } else if (is.call(solution[[i]])) {
    missing_call(that_call = solution[[i]][1],
                 this = renest(user))
  } else {
    missing_value(solution[[i]])
  }
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

