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
            return(mismatched(user, solution, i))
      } else {
        # TO DO: call order_calls on these! 
        # And don't delete first argument name
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

# classify mismatched elements
mismatched <- function(user, solution, i) {
  if (i != 1 && is.call(user) && is.call(solution)) {
    wrong_call(this_call = user[i], 
               this = renest(user[seq_len(i-1)]), 
               that_call = solution[i])
  } else {
    wrong_value(this = user[[i]], that = solution[[i]])
  }
}

