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
  
  if (is.call(user)) user <- deparse(user)
  if (is.call(solution)) solution <- deparse(solution)
  
  if (is.null(.name)) {
    return(glue::glue(
      "I expected {solution} where you wrote {user}. ",
      "Please try again."
    ))
  } else {
    return(glue::glue(
      "I expected {.name} = {solution} where you wrote ",
      "{.name} = {user}. Please try again."
    ))
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

did_not_expect_infix_after <- function(this, that, what) {
  if (is.call(this)) this <- deparse(this)
  if (is.call(that)) that <- deparse(that)
  if (is.call(what)) that <- deparse(what)
  
  return(glue::glue(
    "I expected you to call {what} on {this} instead of using {that} after it. ",
    "Please try again."
  ))
}

expected_infix_after <- function(this, that, what) {
  if (is.call(this)) this <- deparse(this)
  if (is.call(that)) that <- deparse(that)
  if (is.call(what)) that <- deparse(what)
  
  return(glue::glue(
    "I expected you to use {that} after {this} instead of calling {what} on it. ",
    "Please try again."
  ))
}

not_a_call <- function(code) {
  glue::glue("{deparse(code)} is not a recognized call. Please try again.")
}

expected_you_to_call <- function(that, this, .name) {
  if (is.call(this)) this <- deparse(this)
  if (is.call(that)) that <- deparse(that)

  glue::glue("I expected you to call {this} on {that}. Please try again.")
}