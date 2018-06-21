compare_pipes <- function(user,
                          solution,
                          .name = NULL) {
  
  # Did the user write too little?
  if (is_pipe(solution) || is_infix(solution)) {
    .user <- unpipe_all(user)
    .solution_rhs <- unpipe_all(solution[[2]])
    if (length(.user) == length(.solution_rhs) && .user == .solution_rhs) {
      missing <- paste(deparse(solution[[1]]), deparse(solution[[3]][1]))
      return(expected_after(user[[3]], missing, .name))
    }
  }
  
  # Did the user write too much?
  .user_rhs <- unpipe_all(user[[2]])
  .solution <- unpipe_all(solution)
  if (length(.user_rhs) == length(.solution) && .user_rhs == .solution) {
    excess <- paste(deparse(user[[1]]), deparse(user[[3]]))
    return(did_not_expect_after(user[[2]], excess))
  }
  
  # Is the lhs correct?
  if (user[[2]] != solution[[2]]) {
    return(detect_code_mistakes(user[[2]], solution[[2]]))
  }
  
  # Is the rhs correct?
  if (is_pipe(solution) && user[[3]] != solution[[3]]) {
    return(detect_code_mistakes(user[[3]], solution[[3]]))
    
  } else {
    user <- unpipe(user)
    solution <- unpipe(solution)
    if (length(user) != length(solution) ||
        user != solution) {
      return(detect_code_mistakes(user, solution))
    }
  }
}

