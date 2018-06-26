# Each case should have a function that takes both strings and quotes, and hence
# is easy to drop into testthat. ("that" always refers to correct/solution code,
# because it is not at hand in the submission. "this" always refers to
# incorrect/user code, which is always at hand). Cases:

# missing argument
missing_argument <- function(this, that, that_name = NULL) {
  this <- prep(this)
  that <- prep(that)
  if (!is.null(that_name) && that_name != "") 
    that <- paste(that_name, "=", that)
  
  glue::glue("Your call to {this} should include the argument {that} ",
             "(perhaps with some arguments of its own). You may have ",
             "referred to it in a different way, or left out an important ",
             "argument name.")
}

# surplus argument
surplus_argument <- function(this_call, this, this_name = NULL) {
  this_call <- prep(this_call)
  this      <- prep(this)
  if (!is.null(this_name) && this_name != "") 
    this <- paste(this_name, "=", this)
  
  glue::glue("I did not expect your call to {this_call} to ",
             "include {this}. You ",
             "may have included an unnecessary argument, or you ",
             "may have left out or misspelled an important ",
             "argument name.")
}

# wrong value
wrong_value <- function(this, that, that_name = NULL, this_name = NULL) {
  this <- prep(this)
  that <- prep(that)
  
  if (!is.null(that_name) && that_name != "") 
    that <- paste(that_name, "=", that)
  if (!is.null(this_name) && this_name != "") 
    this <- paste(this_name, "=", this)
  
  glue::glue("I expected {that} where you wrote {this}.")
}

prep <- function(text) {
  if (is.call(text)) text <- text[1]
  if (!is.character(text)) text <- deparse(text)
  text
}






####

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



# surplus call
surplus_call <- function(this_call, this) {
  this_call <- prep(this_call)
  this <- prep(this)
  
  glue::glue("I did not expect you to call {this_call} ",
             "(or anything else) on {this}.")
}



# wrong call
wrong_call <- function(this_call, this, that_call) {
  this_call <- prep(this_call)
  this      <- prep(this)
  that_call <- prep(that_call)
  
  glue::glue("I expected you to call {that_call} on {this} ",
             "(instead of {this_call}).")
}

