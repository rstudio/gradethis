.infixes <- c("+", "-", "*", "/", "^", "%%", "%/%", "%in%", "%>%")

is_infix <- function(x) as.character(x[[1]]) %in% .infixes

#' detect_mistakes_with_infix
#'
#' @param user 
#' @param solution 
#' @param .name 
detect_mistakes_with_infix <- function(user,
                                       solution,
                                       .name = NULL) {
  
  # Does user not use infix? (can happen during recursion)
  if (!is_infix(user)) {
    
    if (!is_infix(solution)) {
      return(detect_mistakes(user, solution, name))
    } else if (user == solution[[2]]) {
      missing <- paste(deparse(solution[[1]]), deparse(solution[[3]]))
      return(expected_after(user, missing, .name))
    } else {
      return(detect_mistakes_with_infix(user, solution[[2]], .name))
    }

  # If user uses infix, does solution not?
  } else if (!is_infix(solution)) {
    if (user[[2]] == solution) {
      excess <- paste(deparse(user[[1]]), deparse(user[[3]]))
      return(did_not_expect_after(solution, excess, .name))
    } else {
      return(detect_mistakes_with_infix(user[[2]], solution, .name))
    }
  }

  # does solution use the same infix operator?
  if (user[[1]] != solution[[1]]) {
    return(does_not_match(user, solution, .name))
  }

  # Infix operators isolate the last element and combine the rest.
  # So we compare elements recursively from the last to the first.
  if (user[[3]] != solution[[3]]) {
    if (user == solution[[2]]) {
      missing <- paste(deparse(solution[[1]]), deparse(solution[[3]]))
      return(expected_after(user[[3]], missing, .name))
    }

    if (user[[2]] == solution) {
      excess <- paste(deparse(user[[1]]), deparse(user[[3]]))
      return(did_not_expect_after(solution[[3]], excess, .name))
    }

    if (user[[2]] == solution[[2]]) {
      return(detect_mistakes_with_infix(user[[3]], solution[[3]], .name))
    }
  }

  if (user[[2]] != solution[[2]]) {
    return(detect_mistakes_with_infix(user[[2]], solution[[2]], .name))
  }
}

expected_after <- function(this, that, .name = NULL) {
  if (is.call(this)) this <- deparse(this)
  if (is.call(that)) that <- deparse(that)

  if (is.null(.name)) {
    return(glue::glue(
      "I expected your code to include {that} after {this}. ",
      "Please try again."
    ))
  } else {
    return(glue::glue(
      "I expected your code to include {.name} = {that} after {this}. ",
      "Please try again."
    ))
  }
}

did_not_expect_after <- function(this, that, .name = NULL) {
  if (is.call(this)) this <- deparse(this)
  if (is.call(that)) that <- deparse(that)

  if (is.null(.name)) {
    return(glue::glue(
      "I did not expect your code to include {that} after {this}. ",
      "Please try again."
    ))
  } else {
    return(glue::glue(
      "I did not expect your code to include {.name} = {that} after {this}. ",
      "Please try again."
    ))
  }
}
