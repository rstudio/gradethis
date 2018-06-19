compare_pipes <- function(user,
                          solution,
                          .name = NULL) {
  
  # Did the user write too little?
  if (is_infix(solution)) {
    u <- unpipe_all(user)
    s2 <- unpipe_all(solution[[2]])
    if (length(u) == length(s2) && u == s2) {
      missing <- paste(deparse(solution[[1]]), deparse(solution[[3]]))
      return(expected_after(user[[3]], missing, .name))
    }
  }
  
  # Did the user write too much?
  u2 <- unpipe_all(user[[2]])
  s <- unpipe_all(solution)
  if (length(u2) == length(s) && u2 == s) {
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
    if (user != solution) return(detect_code_mistakes(user, solution))
  }
}

