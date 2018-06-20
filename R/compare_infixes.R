compare_infixes <- function(user,
                            solution,
                            .name = NULL) {
  
  .solution <- unpipe_all(solution)
  .user <- unpipe_all(user)
  .user_lhs <- unpipe_all(user[[2]])
  
  # Did the user write too much?
  if (length(.user_lhs) == length(.solution) &&
      .user_lhs == .solution) {
    excess <- paste(deparse(user[[1]]), deparse(user[[3]]))
    return(did_not_expect_after(user[[2]], excess))
  } 
  
  if (is_infix(solution)) {
    .solution_lhs <- unpipe_all(solution[[2]])
    
    # Did the user write too little?
    if (length(.user) == length(.solution_lhs) &&
        .user == .solution_lhs) {
      missing <- paste(deparse(solution[[1]]), deparse(solution[[3]]))
      return(expected_after(user[[3]], missing, .name))
    }
    
    # Did the user write the wrong thing on the lhs?
    if (length(.user_lhs) != length(.solution_lhs) ||
        .user_lhs != .solution_lhs) {
      return(detect_code_mistakes(user[[2]], solution[[2]]))
    }
    
    # Did the user use the correct infix operator?
    if (user[[1]] != solution[[1]]) {
      return(does_not_match(user, solution))
    }
    
    # Did the user write the wrong thing on the rhs?
    if (length(user[[3]]) != length(solution[[3]]) ||
        user[[3]] != solution[[3]]) {
      return(detect_code_mistakes(user[[3]], solution[[3]]))
    }
  
  # Did the solution use an infix operator?
  } else {
    return(did_not_expect_infix_after(user[[2]], user[[1]], solution[[1]])) 
  }
}

order_infixes <- function(code) {
  if(is_infix(code)) {
    code <- as.list(code[c(2, 1, 3)])
    return(unlist(purrr::map(code, order_infixes)))
  }
  unlist(code)
}
