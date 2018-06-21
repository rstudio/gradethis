compare_calls <- function(user, 
                          solution, 
                          .name = NULL) {
  
  # ensure the user called a valid function
  if (is.atomic(user[[1]]) ||
      !exists(deparse(user[[1]])) ||
      !is.function(get(deparse(user[[1]])))) {
    return(not_a_call(user[[1]]))
  }
  
  # calls should be treated the same whether
  # or not the solution uses a pipe
  solution <- unpipe(solution)
  
  # Did the solution expect an infix operator? If so...
  if (is_infix(solution)) {
    
    # Did the user not write enough?
    if (user == solution[[2]]) {
      missing <- paste(deparse(solution[[1]]), deparse(solution[[3]]))
      return(expected_after(user[[3]], missing, .name))
      
      # Or use a call when they should've used an infix?
    } else if (user[[2]] == solution[[2]]) {
      return(expected_infix_after(user[[2]], solution[[1]], user[[1]]))
    }
  }
  
  # ensure that the submission and 
  # the solution use the same call
  if (user[[1]] != solution[[1]]) {
    return(does_not_match(user, solution, .name))
  }
  
  # match unnamed arguments to names as R would, then 
  # compare the named elements in the submission to 
  # the named elements in the solution one at a time
  user <- pryr::standardise_call(user)
  solution <- pryr::standardise_call(solution)
  named_args <- union(names(user), names(solution))
  named_args <- named_args[named_args != ""]
  first_name <- named_args[1] 
  
  for (name in named_args) {
    
    # it would be distracting to name the
    # first argument when giving feedback (e.g. .x)
    if (name == first_name) {
      message <- detect_code_mistakes(user[[name]], solution[[name]])
    } else {
      message <- detect_code_mistakes(user[[name]], solution[[name]], name)
    }
    if (!is.null(message)) return(message)
  }
  
  # Some arguments in the submission and solution may still be 
  # unnamed. These arguments were not named by the author, nor 
  # matched to a name by R. Get these arguments and then compare 
  # them to each other one at a time by the order that they 
  # appear in.
  if (is.null(names(user))) {
    user_unnamed <- user
  } else {
    user_unnamed <- user[names(user) == ""][-1]
  }
  if (is.null(names(solution))) {
    solution_unnamed <- solution
  } else {
    solution_unnamed <- solution[names(solution) == ""][-1]
  }
  
  max_length <- max(length(user_unnamed), length(solution_unnamed))
  
  for (i in seq_len(max_length)) {
    message <- detect_code_mistakes(user_unnamed[[i]], solution_unnamed[[i]])
    if (!is.null(message)) return(message)
  }
}
