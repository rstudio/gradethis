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
    if (i > length(user)) {
      return(missing_argument(this = user[[i-1]], 
                              that = solution[[i]], 
                              that_name = names(solution[i])))
    }
    
    # Did the user write too much?
    if (i > length(solution)) {
      return(surplus_argument(this_call = user[[i - 1]][1], 
                              this_name = names(user[i]),
                              this = ifelse(is.call(user[[i]]), 
                                            renest(user[i:length(user)]), 
                                            user[[i]])
                              ))
    }
    
    # Does the user code not match the solution code?
    if (user[[i]] != solution[[i]]) {
      return(isolate_mismatch(user, solution, i))
    }
  }
  NULL
}
    
isolate_mismatch <- function(user, solution, i) {
  
  # We've honed in on the error when we can narrow 
  # it down to a single incorrect user element 
  # matched to a single correct solution element
  if (length(user[[i]]) == 1 &&
      length(solution[[i]]) == 1) {
    
    # errors that involve an infix operator make more 
    # sense if the explanation refers to the operator
    if (i == 2 && is_infix(user[[1]]) && length(user[[1]]) == 1) {
      wrong <- paste(deparse(user[[1]][[1]]), deparse(user[[2]]))
      
      # If the error is the name of a call, don't muddle
      # things by referring to the call's arguments
    } else if (is.call(user[[i]])) {
      wrong <- ifelse(i == 1, 
                      user[[i]][1],
                      renest(user[i:length(user)]))
    } else {
      wrong <- user[[i]]
    }
    
    if (i == 2 && is_infix(solution[[1]]) && length(solution[[1]]) == 1) {
      right <- paste(deparse(solution[[1]][[1]]), deparse(solution[[2]]))
      
    } else if (is.call(solution[[i]])) {
      right <- solution[[i]][1]
    } else {
      right <- solution[[i]]
    }
    
    return(wrong_value(this = wrong, 
                       that = right,
                       this_name = names(user[i]),
                       that_name = names(solution[i])))

    # If we cannot do this, we are working with two 
    # multipart calls and we need to identify which 
    # elements of the calls do not align (here we 
    # rely heavily on the fact that both calls have 
    # been previously standardized)
  } else {
    this_name <- names(user[i])
    that_name <- names(solution[i])
    user_call <- user[[i]]
    solution_call <- solution[[i]]
    
    # First check that the calls match. 
    if (user_call[[1]] != solution_call[[1]]) {
      wrong <- ifelse(is_infix(user_call[1]),
                      renest(user[i:length(user)]),
                      user_call[[1]][1])
      right <- ifelse(is_infix(solution_call[1]),
                      renest(solution[i:length(solution)]),
                      solution_call[[1]][1])
      
      return(wrong_value(this = wrong, 
                         that = right,
                         this_name = this_name,
                         that_name = that_name))
      
      # Then inspect the arguments.
    } else {
      return(detect_mistakes(user_call, solution_call))
    #   for (j in seq_along(user_call)) {
    #     if (j == 1) next
    #     
    #     # Did the user leave out an argument?
    #     if (j > length(user_call)) {
    #       return(missing_argument(this = user_call[[1]][1], 
    #                               that = solution_call[[j]], 
    #                               that_name = names(solution_call[j])))
    #     }
    #     
    #     # Did the user include an extra argument?
    #     if (j > length(solution_call)) {
    #       return(surplus_argument(this_call = user_call[[1]][1], 
    #                               this = user_call[[j]]),
    #                               this_name = names(user_call[j]))
    #     }
    #       
    #     # Do two arguments conflict? They may themselves 
    #     # contain an expression that we should drill into.
    #     if (user_call[[j]] != solution_call[[j]]) 
    #       return(detect_mistakes(user_call, solution_call))
    #   }
    }
  }
  stop("Mismatch detected, but not spotted.")
}



