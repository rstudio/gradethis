# Each case should have a function that takes both strings and quotes, and hence
# is easy to drop into testthat. ("that" always refers to correct/solution code,
# because it is not at hand in the submission. "this" always refers to
# incorrect/user code, which is always at hand). Cases:



extra_answer <- function(this_line) {
  glue::glue_data(
    list(
      this_line = prep(this_line)
    ),
    "I didn't expect the call {this_line} in your answer. Please remove it and resubmit your work."
  )

}

missing_answer <- function(this_prior_line) {
  glue::glue_data(
    list(
      this_prior_line = prep(this_prior_line)
    ),
    "I expected another call after {this_prior_line}. Did you forget to write one?"
  )
}




# bad argument name
bad_argument_name <- function(this_call,
  this,
  this_name,
  enclosing_call = NULL,
  enclosing_arg = NULL) { # only if the user supplied one (to match user code)

  # f(1, g(1, h(b = i(1))))
  # f(1, a = g(1, a = h(ba = i(1)), bb = i(2)))

  # In f(1, g(1, h(b = i(1)))), h() accepts more than one argument that begins with b.
  # As a result, R cannot figure out which argument you want to pass i(1) to.
  # Check how you spelled b, or write out the full argument name.

  # {intro}{this_call} accepts more than one argument that begins with {this_name}.
  # As a result, R cannot figure out which argument you want to pass {this} to.
  # Check how you spelled {this_name}, or write out the full argument name.

  intro <- build_intro(.call = enclosing_call, .arg = enclosing_arg)

  this <- prep(this)
  this_call <- prep(this_call)

  if (grepl("\\(\\)", this))
    this <- paste("a call to", this)

  glue::glue_data(
    list(
      intro = intro,
      this_call = this_call,
      this_name = this_name,
      this = this
    ),
    "{intro}{this_call} accepts more than one argument name that begins ",
    "with `{this_name}`. As a result, R cannot figure out which ",
    "argument you want to pass {this} to. Check how you spelled ",
    "`{this_name}`, or write out the full argument name."
  )
}

# duplicate_name
duplicate_name <- function(this_call,
  this_name,
  enclosing_call = NULL,
  enclosing_arg = NULL) {

  # f(a = 1, a = 2)
  # f(a = 1)

  # "You passed multiple arguments named a to f(), which will cause "
  # "an error. Check your spelling, or remove one of the arguments."

  # "You passed multiple arguments named {this_name} to {this_call}, which will cause "
  # "an error. Check your spelling, or remove one of the arguments."

  this_call <- prep(this_call)
  this_name <- prep(this_name)

  intro <- build_intro(.call = enclosing_call, .arg = enclosing_arg)

  glue::glue_data(
    list(
      intro = intro,
      this_call = this_call,
      this_name = this_name
    ),
    "You passed multiple arguments named {this_name} ",
    "to {this_call}, which will cause an error. ",
    "Check your spelling, or remove one of the arguments."
  )
}

# WHAT TO DO IF THE MISSING ARGUMENT DOESN'T HAVE A NAME IN THE SOLUTION?
# missing argument
missing_argument <- function(this_call,
  that_name = NULL,
  enclosing_call = NULL,
  enclosing_arg = NULL) {

  # f(1, g(1, h(i(1))))
  # f(1, a = g(1, a = h(a = i(1)), b = i(2)))

  # "In g(1, h(i(1))), Your call to h() should include b",
  # "as one of its arguments. You may have referred to it ",
  # "in a different way, or left out an important argument name."

  # "{intro}Your call to {this_call} should include {that_name} ",
  # "as one of its arguments. You may have referred to it ",
  # "in a different way, or left out an important argument name."

  intro <- build_intro(.call = enclosing_call, .arg = enclosing_arg)
  your_char <- ifelse(intro == "", "Y", "y")

  this_call <- prep(this_call)
  that_name <- prep(that_name)

  if (grepl("\\(\\)", that_name)) {
    that_name <- paste0("an argument, possibly unnamed, that calls ", that_name, ".")
  } else {
    that_name <- paste(that_name, "as one of its arguments.")
  }

  glue::glue_data(
    list(
      intro = intro,
      this_call = this_call,
      that_name = that_name
    ),
    "{intro}{your_char}our call to {this_call} should include {that_name} ",
    "You may have misspelled an argument name, ",
    "or left out an important argument."
  )
}

# surplus argument
surplus_argument <- function(this_call,
  this,
  this_name = NULL,
  enclosing_call = NULL,
  enclosing_arg = NULL) {

  # f(1, g(1, h(1, b = i(1))))
  # f(1, a = g(1, a = h(a = 1)))

  # "In g(1, h(1, i(1))), I did not expect your call to h() to ",
  # "include b = i(1). You ",
  # "may have included an unnecessary argument, or you ",
  # "may have left out or misspelled an important ",
  # "argument name."

  # "{intro}I did not expect your call to {this_call} to ",
  # "include {this}. You ",
  # "may have included an unnecessary argument, or you ",
  # "may have left out or misspelled an important ",
  # "argument name."

  intro <- build_intro(.call = enclosing_call, .arg = enclosing_arg)

  this_call <- prep(this_call)
  this      <- prep(this)

  if (!is.null(this_name) && this_name != "")
    this <- md_code_prepend(paste(this_name, "= "), this)

  glue::glue_data(
    list(
      this = this,
      this_call = this_call
    ),
    "{intro}I did not expect your call to {this_call} to ",
    "include {this}. You ",
    "may have included an unnecessary argument, or you ",
    "may have left out or misspelled an important ",
    "argument name."
  )
}


# partial matching
pmatches_argument_name <- function(this_call,
  this,
  this_name = NULL,
  correct_name = NULL,
  enclosing_call = NULL,
  enclosing_arg = NULL) {


  # "{intro}I did not expect your call to {this_call} to ",
  # "include {this}. You ",
  # "may have included an unnecessary argument, or you ",
  # "may have left out or misspelled an important ",
  # "argument name."
  # intro <- build_intro(.call = enclosing_call, .arg = enclosing_arg)


  # "This code seems correct, but please write with full parameter(s) names."
  # "You wrote {this} please rewrite with {correct_name} ."
  # "You wrote {this} please rewrite with {correct_name} ."


  this_call <- prep(this_call)
  this <- lapply(this, prep) #yes devrait etre quoted
  this_user <- this

  if (!is.null(this_name)) {
    this_name <- paste(this_name, "= ")
    this_user <- purrr::map2(this_name, this, md_code_prepend)
  }

  if (!is.null(correct_name)) {
    correct_name <- paste(correct_name, "= ")
    correct_name <- purrr::map2(correct_name, this, md_code_prepend)
  }

  intro  <- "This code seems correct, but please write using full argument(s) names:\n\n"
  msg <- glue::glue_data(
    list(
      this = this_user,
      correct_name = correct_name,
      this_call = this_call
    ),
    "- Where you wrote {this}, please use the full formal name {correct_name}."
  )

  glue::glue_data(
    list(
      intro = intro,
      msg = msg
    ),
    "{intro}{paste0(msg, collapse = '\n')}"
  )
}

# too_many_matches
too_many_matches <- function(this_call,
  that_name,
  enclosing_call = NULL,
  enclosing_arg = NULL) {

  # f(1, g(1, h(b = i(1), ba = 2)))
  # f(1, a = g(1, a = h(bab = 1)))

  # "Double check the argument names you are using. ",
  # "In g(1, h(b = i(1), ba = 2)), h() accepts an argument named bab. More than one of your argument names will ",
  # "be matched to bab, which will cause an error. Try ",
  # "writing out the full argument names."

  # "Double check the argument names you are using. ",
  # "{intro}{this_call} accepts an argument named {that} and it ",
  # "looks like more than one of your argument names will ",
  # "be matched to {that}, which will cause an error. Try ",
  # "writing out the full argument names."

  this_call <- prep(this_call)
  that_name <- prep(that_name)

  intro <- build_intro(.call = enclosing_call, .arg = enclosing_arg)

  glue::glue_data(
    list(
      intro = intro,
      this_call = this_call,
      that_name = that_name
    ),
    "{intro}{this_call} accepts an argument named {that_name}. ",
    "More than one of your argument names in {this_call} will ",
    "be matched to {that_name}, which will cause an error. Try ",
    "writing out the full argument names."
  )
}

# wrong call
wrong_call <- function(this,
  that,
  this_name = NULL,
  enclosing_call = NULL) {

  # f(1, g(1, h(a = i(1))))
  # f(1, a = g(1, a = h(a = j(1))))

  # "g(1, h(i(1))), I expected you to call a = j() where you called a = i()."

  # "{intro}I expected you to {action} {that} where you called {this}."

  intro <- build_intro(.call = enclosing_call)

  that_original <- that
  this <- prep(this)
  that <- prep(that)

  if (!is.null(this_name) && this_name != "") {
    that <- md_code_prepend(paste(this_name, "= "), that)
    this <- md_code_prepend(paste(this_name, "= "), this)
  }

  action <-
    if (is_infix_assign(that_original)) {
      "assign something to something else with"
    } else {
      "call"
    }

  glue::glue_data(
    list(
      this = this,
      that = that,
      action = action
    ),
    "{intro}I expected you to {action} {that} where you called {this}."
  )
}

# wrong value for wrong value and wrong call, the enclosing argument is the
# argument that appears before the call or value. It should be passed to
# this_name
wrong_value <- function(this,
  that,
  this_name = NULL,
  enclosing_call = NULL
) {
  if (is_missing(this)) {
    this <- NULL
  }

  # f(1, g(1, h(1)))
  # f(1, a = g(1, a = h(2)))

  # "h(1), I expected 2 where you wrote 1."

  # "{intro}I expected {that} where you wrote {this}."

  intro <- build_intro(.call = enclosing_call)

  expected <- "expected"
  if (length(this) > length(that)) {
    expected <- "didn't expect"
    that <- this
    this <- NULL
  }

  where <- " where you wrote "

  that_original <- that
  that <- prep(that)

  if (is.null(this)) {
    intro <- ""
    this <- build_intro(enclosing_call %||% that_original, .open = "", .close = "")
  } else {
    this <- prep(this)
  }

  if (!is.null(this_name) && this_name != "") {
    that <- md_code_prepend(paste(this_name, "= "), that)
    this <- md_code_prepend(paste(this_name, "= "), this)
  }

  # NOTE: infix operators that are calls like `<-` also
  # need to be accounted for but perhaps there's a cleaner
  # solution than tacking on more greps.
  action <-
    if (is_infix_assign(that_original)) {
      "you to assign something to something else with "
    } else if (grepl("\\(\\)", that)) {
      "you to call "
    }

  glue::glue_data(
    list(
      intro = intro,
      expected = expected,
      that = that,
      where = if (!identical(this, "")) where else "",
      this = this,
      action = action %||% ""
    ),
    "{intro}I {expected} {action}{that}{where}{this}."
  )
}

prep <- function(text) {
  # NOTE: `[` does not work well for assign `<-` and would
  # grab whole expression ending up with: NULL <- NULL.
  # this extra condition to use `[[` works, but requires further
  # investigation for a cleaner solution.
  if (is_infix(text)) {
    text <- text[[1]]
  } else if (is.call(text) && !is_pipe(text)) {
    text <- text[1]
  } else if (is.pairlist(text)) {
    return(prep_function_arguments(text))
  }
  paste0("`", deparse_to_string(text), "`")
}

build_intro <- function(.call = NULL, .arg = NULL, .open = "In ", .close = ", ") {
  is_call_fn_def <- is_function_definition(.call)

  if (!is.null(.call)) {
    .call_str <- deparse_to_string(.call)
    if (!is.null(.arg) && !identical(.arg, "")) {
      .call_str <- paste(.arg, "=", .call_str)
    }
    if (is_call_fn_def) {
      # strip function body
      .call_str <- sub("^(function\\(.+?\\))(.+)$", "\\1", .call_str)
    }
    if (nchar(.call_str) > 80) {
      # too much context, the intro is too long to be helpful
      return("")
    }
    intro <- glue::glue("{.open}`{.call_str}`{.close}")
  } else {
    intro <- ""
  }
  intro
}

prep_function_arguments <- function(arg_list) {
  args <- names(arg_list)
  values <- purrr::map_chr(arg_list, function(arg_value) {
    if (arg_value == quote("")) return("")
    paste(" =", deparse(arg_value))
  })
  s <- if (length(args) == 1) " " else "s "
  paste0("argument", s, paste0("`", args, values, "`", collapse = ", "))
}

md_code_prepend <- function(prefix, x) {
  if (length(x) > 1) {
    return(purrr::map(x, ~ md_code_prepend(prefix, .x)))
  }
  stopifnot(length(prefix) == 1, length(x) == 1)
  if (grepl("^`", x)) {
    # remove leading code backtick if present in the string
    x <- gsub("^`", "", x)
    # add leading code backtick if _not_ present in the prefix
    if (!grepl("^`", prefix)) {
      prefix <- paste0("`", prefix)
    }
  }
  paste0(prefix, x)
}
