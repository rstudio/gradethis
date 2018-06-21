compare_atomics_or_names <- function(user,
                                     solution,
                                    .name = NULL) {
  # Did the user write too little? 
  # How can you tell?
  
  # If the solution is a pipe, the submission will 
  # appear as the first element of the rhs of the pipe
  if (is_pipe(solution)) {
    message <- detect_code_mistakes(user, solution[[2]], .name)
    if (is.null(message)) {
      missing <- paste(deparse(solution[[1]]), deparse(solution[[3]][1]))
      return(expected_after(user, missing, .name))
    } else return(message)
    
    # If the solution is an infix operator, the submission 
    # will appear as the first element in the operator chain
  } else if (is_infix(solution)) {
      solution <- order_infixes(solution)
      message <- detect_code_mistakes(user, solution[[1]], .name)
      if (is.null(message)) {
        missing <- paste(deparse(solution[[2]]), deparse(solution[[3]]))
        return(expected_after(user, missing, .name)) 
      } else return(message)
      
    # If the solution is a function, the submission 
    # will appear as the first argument of the function
  } else if (is.call(solution)) {
    message <- detect_code_mistakes(user, solution[[2]])
    if (is.null(message)) {
      func <- paste0(deparse(solution[[1]]), "()")
      return(expected_you_to_call(user, func, .name)) 
    } else return(message)
    
  } else if (user != solution) {
    return(does_not_match(user, solution, .name))
  } else {
    NULL
  }
}