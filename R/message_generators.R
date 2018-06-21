expected <- function(this, .name = NULL) {
  if (is.null(.name)) {
    return(glue::glue(
      "I expected your code to include {deparse(this)}. You may have ",
      "referred to it in a different way, or left out an important argument name. ",
      "Please try again."
    ))
  } else {
    return(glue::glue(
      "I expected your code to include {.name} = {deparse(this)}. ",
      "You may have referred to it in a different way, or left out an important ",
      "argument name. Please try again."
    ))
  }
}

did_not_expect <- function(that, .name = NULL) {
  if (is.null(.name)) {
    return(glue::glue(
      "I did not expect your code to include {deparse(that)}. ",
      "You may have included an unnecessary value, or you may have left ",
      "out an important argument name. Please try again."
    ))
  } else {
    return(glue::glue(
      "I did not expect your code to include {.name} = {deparse(that)}. ",
      "You may have included an unnecessary value, or you may have used the wrong ",
      "argument name. Please try again."
    ))
  }
}

does_not_match <- function(user, solution, .name = NULL) {
  if (length(solution) > 1) solution <- solution[[1]]
  if (length(user) > 1) user <- user[[1]]
  
  if (is.null(.name)) {
    return(glue::glue(
      "I expected {deparse(solution)} where you wrote {deparse(user)}. ",
      "Please try again."
    ))
  } else {
    return(glue::glue(
      "I expected {.name} = {deparse(solution)} where you wrote ",
      "{.name} = {deparse(user)}. Please try again."
    ))
  }
}
