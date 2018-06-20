compare_infixes <- function(user,
                            solution,
                            .name = NULL) {
  
  # It is useful to check infix code in the order 
  # that the user writes it, which is not the order 
  # in which R parses it
  user <- order_infixes(user)
  solution <- order_infixes(solution)
  
  max_length <- max(length(user), length(solution))
  
  for (i in seq_len(max_length)) {
    
    # Did user write too much?
    if (i > length(solution)) {
      excess <- paste(purrr::map(user[seq(i, to = length(user))], deparse), collapse = " ")
      return(did_not_expect_after(user[[i - 1]], excess))
      
    # Did user write too little?
    } else if (i > length(user)) {
      if (i < length(solution) &&
          as.character(solution[[i]]) %in% c(.infixes, .pipes)) {
        missing <- paste(deparse(solution[[i]]), deparse(solution[[i + 1]]))
      } else {
        missing <- paste(deparse(solution[[i]]))
      }
      return(expected_after(user[[i - 1]], missing))  
    }
    
    message <- detect_code_mistakes(user[[i]], solution[[i]])
    if (!is.null(message)) return(message)
  }
}

# Returns a call to an infix operator (+, -, etc.) as a list of symbols that
# appear in the order that the user writes them. From the student's point of
# view, this is the most straightforward way to check them. Returns the results
# as a list that can be iterated over.
order_infixes <- function(code) {
  code <- infix_orderer(code)
  if (is.call(code)) code <- list(code)
  code
}

infix_orderer <- function(code) {
  if(is_infix(code)) {
    code <- as.list(code[c(2, 1, 3)])
    return(unlist(purrr::map(code, infix_orderer)))
  }
  code
}
