strict_check <- function(user = NULL, solution = NULL, success = "Correct!") {
  message <- detect_mistakes(user, solution)
  if (is.null(message)) return(success)
  else return(message)
}

detect_mistakes <- function(user, solution, .name = NULL) {

  # unmatched
  if (is.null(user) && !is.null(solution)) 
    return(expected(solution, .name))
  else if (is.null(solution) && !is.null(user)) 
    return(did_not_expect(user, .name))

  # atomic or name
  else if (is.atomic(user) || is.name(user)) {
    if (user != solution) return(does_not_match(user, solution, .name))

  # call (or pairlist?)
  } else {
    # ensure the same call
    if (user[[1]] != solution[[1]]) return(does_not_match(user, solution, .name))

    # ensure the same named/matched arguments
    user <- pryr::standardise_call(user)
    solution <- pryr::standardise_call(solution)
    named_args <- union(names(user), names(solution))
    named_args <- named_args[named_args != ""]
    for (name in named_args) {
      message <- detect_mistakes(user[[name]], solution[[name]], name)
      if (!is.null(message)) return(message)
    }

    # ensure the same unnamed arguments (matched by position)
    if (is.null(names(user))) user_unnamed <- user
    else user_unnamed <- user[names(user) == ""][-1]
    if (is.null(names(solution))) solution_unnamed <- solution
    else solution_unnamed <- solution[names(solution) == ""][-1]

    if (length(user_unnamed) < length(solution_unnamed))
      length(user_unnamed) <- length(solution_unnamed)
    if (length(solution_unnamed) < length(user_unnamed))
      length(solution_unnamed) <- length(user_unnamed)
    for (i in seq_along(user_unnamed)) {
      message <- detect_mistakes(user[[i]], solution[[i]])
      if (!is.null(message)) return(message)
    }
  }
}

expected <- function(this, .name = NULL) {
  if (is.null(.name)) {
    return(glue::glue("I expected your code to include {deparse(this)}. You may have ",
      "referred to it in a different way, or left out an important argument name. ",
      "Please try again."))
  } else {
    return(glue::glue("I expected your code to include {.name} = {deparse(this)}. ",
      "You may have referred to it in a different way, or left out an important ",
      "argument name. Please try again."))
  }
}

did_not_expect <- function(that, .name = NULL) {
  if (is.null(.name)) {
    return(glue::glue("I did not expect your code to include {deparse(that)}. ",
      "You may have included an unnecessary value, or you may have left ",
      "out an important argument name. Please try again."))
  } else {
    return(glue::glue("I did not expect your code to include {.name} = {deparse(that)}. ",
      "You may have included an unnecessary value, or you may have used the wrong ",
      "argument name. Please try again."))
  }
}

does_not_match <- function(user, solution, .name = NULL) {
  if (length(solution) > 1) solution <- solution[[1]]
  if (length(user) > 1) user <- user[[1]]
  
  if (is.null(.name)) {
    return(glue::glue("I expected {deparse(solution)} where you wrote {deparse(user)}. ",
      "Please try again."))
  } else {
    return(glue::glue("I expected {.name} = {deparse(solution)} where you wrote ",
      "{.name} = {deparse(user)}. Please try again."))
  }
}
