# Each case should have a function that takes both strings and quotes, and hence
# is easy to drop into testthat. ("that" always refers to correct/solution code,
# because it is not at hand in the submission. "this" always refers to
# incorrect/user code, which is always at hand). Cases:

# missing value
missing_value <- function(that) {
  that <- prep(that)
  glue::glue("I expected your code to include {that}. You may have ",
             "referred to it in a different way, or left out an important ",
            "argument name.")
}

# missing_argument
missing_argument <- function(this_call, that_name = NULL) {
  this_call <- prep(this_call)
  
  if (is.null(that_name)) {
    glue::glue("You should include an argument in your call to ",
               "{this_call}. I do not see one.")
  } else {
    that_name <- prep(that_name)
  
    glue::glue("Your call to {this_call} should include ",
               "an argument named {that_name}. You may have ",
               "referred to it in a different way, or left ",
               "out an important argument.")
  }
}

# missing call
missing_call <- function(that_call, this) {
  that_call <- prep(that_call)
  this <- prep(this)
  
  glue::glue("I expected you to call {that_call} on {this}.")
}

# surplus value
surplus_value <- function(this) {
  this <- prep(this)
  glue::glue("I did not expect your code to include {this}. ",
             "You may have included an unnecessary value, or you may have left ",
             "out an important argument name.")
}

# surplus argument
surplus_argument <- function(this_call, this_name = NULL, this) {
  this_call <- prep(this_call)
  this      <- prep(this)
  
  if (is.null(this)) {
    glue::glue("I did not expect your call to {this_call} to ",
               "include {this} as an argument. You ",
               "may have included an unnecessary argument, or you ",
               "may have left out or misspelled an important ",
               "argument name.")
  } else {
    this_name <- prep(this_name)
    
    glue::glue("I did not expect your call to {this_call} to ",
               "include the argument {this_name} = {this}. You ",
               "may have included an unnecessary argument, or you ",
               "may have left out or misspelled an important ",
               "argument name.")
  }
}

# surplus call
surplus_call <- function(this_call, this) {
  this_call <- prep(this_call)
  this <- prep(this)
  
  glue::glue("I did not expect you to call {this_call} ",
             "(or anything else) on {this}.")
}

# wrong value
wrong_value <- function(this, that) {
  this <- prep(this)
  that <- prep(that)
  glue::glue("I expected {that} where you wrote {this}.")
}

# wrong call
wrong_call <- function(this_call, this, that_call) {
  this_call <- prep(this_call)
  this      <- prep(this)
  that_call <- prep(that_call)
  
  glue::glue("I expected you to call {that_call} on {this} ",
             "(instead of {this_call}).")
}

prep <- function(text) {
  if (is.name(text) || is.call(text)) {
    text <- deparse(text)
    return(gsub("\\(NULL\\)", "()", text))
  }
  text
}