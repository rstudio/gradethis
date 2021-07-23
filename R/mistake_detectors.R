detect_wrong_value <- function(user, solution, submitted, enclosing_arg, enclosing_call) {
  if (!is.call(user) || !is.call(solution)) {
    if (!identical(user, solution)) {
      if (detect_mismatched_function_arguments(user, solution)) {
        submitted <- as.pairlist(user[setdiff(names(user), names(solution))])
        solution <- as.pairlist(solution[setdiff(names(solution), names(user))])
      }
      
      return(
        wrong_value(
          submitted = submitted,
          solution = solution,
          submitted_name = enclosing_arg,
          enclosing_call = enclosing_call
        )
      )
    }
  }
}

detect_mismatched_function_arguments <- function(user, solution) {
  is.pairlist(user) && 
    is.pairlist(solution) &&
    length(user) != length(solution)
}
