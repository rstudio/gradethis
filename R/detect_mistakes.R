detect_mistakes <- function(user,
                            solution) {
  
  # code should be checked in the order 
  # of evaluation, whether or not the 
  # student (and/or teacher) used a pipe
  user <- order_calls(unpipe_all(user))
  solution <- order_calls(unpipe_all(solution))
  
  max_length <- max(length(user), length(solution))
  
  for (i in seq_len(max_length)) {
    if (i > length(user)) return(missed(solution, i))
    if (i > length(solution)) return(exceeded(user, i))
    
    if (user[[i]] != solution[[i]]) {
      if (length(user[[i]]) == 1 &&
          length(solution[[i]]) == 1) {
        
        # surplus or missing arguments of the correct function 
        # will surface as a mismatch in the first argument
        if (i == 1) return(first_mismatched(user, solution))
        else return(mismatched(user, solution, i))
        
      } else {
        
        # if the element contains multiple parts (or should)
        # see which part caused the error
        return(detect_mistakes(user[[i]], solution[[i]]))
      }
    }
  }
  NULL
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
  } else {
    mismatched(user, solution, 1)
  }
}

# classify mismatched elements
mismatched <- function(user, solution, i) {
  
  # The mismatched element is the second argument of an infix operator
  if (i == 1 && length(user) == 2 && is_infix(user[[2]])) {
    
    # ...and the solution also uses an infix operator
    if (length(solution) == 2 && is_infix(solution[[2]])) {
      
      # ...the same infix operator
      if (solution[[2]][[1]] == user[[2]][[1]]) {
      wrong_value(user[[i]], solution[[i]])
      
      # ...or a different infix operator
      } else {
        incorrect <- paste(deparse(user[[2]]), deparse(user[[1]]))
        correct <- paste(deparse(solution[[2]]), deparse(solution[[1]]))
        wrong_value(incorrect, correct)
      }
      
    # ...or the solution uses a call that is not an infix operator
    } else if (is.call(solution[[1]])) {
        wrong_call(user[[2]][[1]], user[[1]], solution[1])
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

