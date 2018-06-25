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
                              that_name = names(solution[i])))
    
    # Did the user write too much?
    if (i > length(solution)) {
      return(surplus_argument(this_call = user[[i - 1]][1], 
                              this_name = names(user[i]),
                              this = ifelse(is.call(user[[i]]), 
                                            renest(user[i:length(user)]), 
                                            user[[i]])
                              ))
    
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



