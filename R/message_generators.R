# Each case should have a function that takes both strings and quotes, and hence
# is easy to drop into testthat. ("that" always refers to correct/solution code,
# because it is not at hand in the submission. "this" always refers to
# incorrect/user code, which is always at hand). Cases:

# missing value
missing_value <- function(that) {
  that <- prep(that)
  glue::glue("I expected your code to include {that}. You may have ",
             "referred to it in a different way, or left out an important ",
            "argument name. {sample(.encourage, 1)}")
}

# surplus value
surplus_value <- function(this) {
  this <- prep(this)
  glue::glue("I did not expect your code to include {this}. ",
             "You may have included an unnecessary value, or you may have left ",
             "out an important argument name. {sample(.encourage, 1)}")
}


# wrong value
wrong_value <- function(this, that) {
  this <- prep(this)
  that <- prep(that)
  glue::glue("I expected {that} where you wrote {this}. {sample(.encourage, 1)}")
}

# missing_argument
missing_argument <- function(this_call, that_name) {
  this_call <- prep(this_call)
  that_name <- prep(that_name)
  
  glue::glue("Your call to {this_call} should include ",
             "an argument named {that_name}. You may have ",
             "referred to it in a different way, or left ",
             "out an important argument. {sample(.encourage, 1)}")
}

# surplus argument
surplus_argument <- function(this_call, this_name, this) {
  this_call <- prep(this_call)
  this_name <- prep(this_name)
  this      <- prep(this)
  
  glue::glue("I did not expect your call to {this_call} to ",
             "include the argument {this_name} = {this}. You ",
             "may have included an unnecessary argument, or you ",
             "may have left out or misspelled an important ",
             "argument name. {sample(.encourage, 1)}")
}

# missing call
missing_call <- function(that_call, this) {
  that_call <- prep(that_call)
  this <- prep(this)
  
  glue::glue("I expected you to call {that_call} on {this}. {sample(.encourage, 1)}")
}

# surplus call
surplus_call <- function(this_call, this) {
  this_call <- prep(this_call)
  this <- prep(this)
  
  glue::glue("I did not expect you to call {this_call} ",
             "(or anything else) on {this}. {sample(.encourage, 1)}")
}

# wrong call
wrong_call <- function(this_call, this, that_call) {
  this_call <- prep(this_call)
  this      <- prep(this)
  that_call <- prep(that_call)
  
  glue::glue("I expected you to call {that_call} on {this} ",
             "(instead of {this_call}). {sample(.encourage, 1)}")
}

prep <- function(text) {
  if (is.name(text) || is.call(text)) {
    text <- deparse(text)
    return(gsub("\\(NULL\\)", "()", text))
  }
  text
}

# Encouragement messages
.encourage <- c("Please try again.",
                "Give it another try.",
                "Let's try it again.",
                "Try it again; next time's the charm!",
                "Don't give up now, try it one more time.",
                "But no need to fret, try it again.",
                "Try it again. I have a good feeling about this.",
                "Try it again. You get better each time.",
                "Try it again. Perseverence is the key to success.",
                "That's okay: you learn more from mistakes than successes. Let's do it one more time.")
                