# Each case should have a function that takes both strings and quotes, and hence
# is easy to drop into testthat. ("that" always refers to correct/solution code,
# because it is not at hand in the submission. "this" always refers to
# incorrect/user code, which is always at hand). Cases:

# missing argument
missing_argument <- function(this_call, that, that_name = NULL) {
  this_call <- prep(this_call)
  that <- prep(that)

  if (!is.null(that_name) && that_name != "")
    that <- paste(that_name, "=", that)

  if (grepl("\\(\\)", that))
    that <- paste("a call to", that)

  glue::glue("Your call to {this_call} should include {that} ",
             "as one of its arguments. You may have ",
             "referred to it in a different way, or left out an important ",
             "argument name.")
}

# surplus argument
surplus_argument <- function(this_call, this, this_name = NULL) {
  this_call <- prep(this_call)
  this      <- prep(this)
  if (!is.null(this_name) && this_name != "")
    this <- paste(this_name, "=", this)

  glue::glue_data(
    list(
      this = this,
      this_call = this_call
    ),
    "I did not expect your call to {this_call} to ",
    "include {this}. You ",
    "may have included an unnecessary argument, or you ",
    "may have left out or misspelled an important ",
    "argument name."
  )
}

# wrong call
wrong_call <- function(this, 
                       that, 
                       enclosing_call = NULL, 
                       enclosing_arg = NULL) {
  this <- prep(this)
  that <- prep(that)
  
  if(!is.null(enclosing_call)) {
    enclosing_call <- prep(enclosing_call)
    intro <- glue::glue("In {enclosing_call}, ")
  } else {
    intro <- ""
  }
  
  if (!is.null(enclosing_arg) && enclosing_arg != "") {
    that <- paste(enclosing_arg, "=", that)
    this <- paste(enclosing_arg, "=", this)
  }
  
  glue::glue_data(
    list(intro = intro, this = this, that = that),
    "{intro}I expected you to call {that} where you called {this}."
  )
}

# wrong value
wrong_value <- function(this, that, that_name = NULL, this_name = NULL) {
  this <- prep(this)
  that <- prep(that)

  if (!is.null(that_name) && that_name != "")
    that <- paste(that_name, "=", that)
  if (!is.null(this_name) && this_name != "")
    this <- paste(this_name, "=", this)

  if (grepl("\\(\\)", that))
    that <- paste("a call to", that)

  glue::glue_data(
    list(this = this, that = that),
    "I expected {that}; what you wrote was interpreted as {this}."
  )
}

prep <- function(text) {
  if (is.call(text)) text <- text[1]
  if (!is.character(text)) text <- deparse_to_string(text)
  text
}
