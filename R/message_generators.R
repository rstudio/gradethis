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
bad_argument_name <- function(submitted_call,
                              submitted,
                              submitted_name,
                              enclosing_call = NULL,
                              enclosing_arg = NULL) { # only if the user supplied one (to match user code)

  # f(1, g(1, h(b = i(1))))
  # f(1, a = g(1, a = h(ba = i(1)), bb = i(2)))

  # In f(1, g(1, h(b = i(1)))), h() accepts more than one argument that begins with b.
  # As a result, R cannot figure out which argument you want to pass i(1) to.
  # Check how you spelled b, or write out the full argument name.

  # {intro}{submitted_call} accepts more than one argument that begins with {submitted_name}.
  # As a result, R cannot figure out which argument you want to pass {submitted} to.
  # Check how you spelled {submitted_name}, or write out the full argument name.

  intro <- build_intro(.call = enclosing_call, .arg = enclosing_arg)

  submitted <- prep(submitted)
  submitted_call <- prep(submitted_call)

  if (grepl("\\(\\)", submitted))
    submitted <- paste("a call to", submitted)

  glue::glue_data(
    list(
      intro = intro,
      submitted_call = submitted_call,
      submitted_name = submitted_name,
      submitted = submitted
    ),
    "{intro}{submitted_call} accepts more than one argument name that begins ",
    "with `{submitted_name}`. As a result, R cannot figure out which ",
    "argument you want to pass {submitted} to. Check how you spelled ",
    "`{submitted_name}`, or write out the full argument name."
  )
}

# duplicate_name
duplicate_name <- function(submitted_call,
                           submitted_name,
                           enclosing_call = NULL,
                           enclosing_arg = NULL) {

  # f(a = 1, a = 2)
  # f(a = 1)

  # "You passed multiple arguments named a to f(), which will cause "
  # "an error. Check your spelling, or remove one of the arguments."

  # "You passed multiple arguments named {submitted_name} to {submitted_call},
  # "which will cause an error. Check your spelling, or remove one of the arguments."

  submitted_call <- prep(submitted_call)
  submitted_name <- prep(submitted_name)

  intro <- build_intro(.call = enclosing_call, .arg = enclosing_arg)

  glue::glue_data(
    list(
      intro = intro,
      submitted_call = submitted_call,
      submitted_name = submitted_name
    ),
    "You passed multiple arguments named {submitted_name} ",
    "to {submitted_call}, which will cause an error. ",
    "Check your spelling, or remove one of the arguments."
  )
}

# WHAT TO DO IF THE MISSING ARGUMENT DOESN"T HAVE A NAME IN THE SOLUTION?
# missing argument
missing_argument <- function(submitted_call,
                             solution_name = NULL,
                             enclosing_call = NULL,
                             enclosing_arg = NULL) {

  # f(1, g(1, h(i(1))))
  # f(1, a = g(1, a = h(a = i(1)), b = i(2)))

  # "In g(1, h(i(1))), Your call to h() should include b",
  # "as one of its arguments. You may have referred to it ",
  # "in a different way, or left out an important argument name."

  # "{intro}Your call to {submitted_call} should include {solution_name} ",
  # "as one of its arguments. You may have referred to it ",
  # "in a different way, or left out an important argument name."

  intro <- build_intro(.call = enclosing_call, .arg = enclosing_arg)
  your_char <- ifelse(intro == "", "Y", "y")

  submitted_call <- prep(submitted_call)
  solution_name <- prep(solution_name)

  if (grepl("\\(\\)", solution_name)) {
    solution_name <- paste0("an argument, possibly unnamed, that calls ", solution_name, ".")
  } else {
    solution_name <- paste(solution_name, "as one of its arguments.")
  }

  glue::glue_data(
    list(
      intro = intro,
      submitted_call = submitted_call,
      solution_name = solution_name
    ),
    "{intro}{your_char}our call to {submitted_call} should include {solution_name} ",
    "You may have misspelled an argument name, ",
    "or left out an important argument."
  )
}

# surplus argument
surplus_argument <- function(submitted_call,
                             submitted,
                             submitted_name = NULL,
                             enclosing_call = NULL,
                             enclosing_arg = NULL) {

  # f(1, g(1, h(1, b = i(1))))
  # f(1, a = g(1, a = h(a = 1)))

  # "In g(1, h(1, i(1))), I did not expect your call to h() to ",
  # "include b = i(1). You ",
  # "may have included an unnecessary argument, or you ",
  # "may have left out or misspelled an important ",
  # "argument name."

  # "{intro}I did not expect your call to {submitted_call} to ",
  # "include {submitted}. You ",
  # "may have included an unnecessary argument, or you ",
  # "may have left out or misspelled an important ",
  # "argument name."

  intro <- build_intro(.call = enclosing_call, .arg = enclosing_arg)

  submitted_call <- prep(submitted_call)
  submitted      <- prep(submitted)

  if (!is.null(submitted_name) && submitted_name != "")
    submitted <- md_code_prepend(paste(submitted_name, "= "), submitted)

  glue::glue_data(
    list(
      submitted = submitted,
      submitted_call = submitted_call
    ),
    "{intro}I did not expect your call to {submitted_call} to ",
    "include {submitted}. You ",
    "may have included an unnecessary argument, or you ",
    "may have left out or misspelled an important ",
    "argument name."
  )
}


# partial matching
pmatches_argument_name <- function(submitted_call,
                                   submitted,
                                   submitted_name = NULL,
                                   solution_name = NULL,
                                   enclosing_call = NULL,
                                   enclosing_arg = NULL) {


  # "{intro}I did not expect your call to {submitted_call} to ",
  # "include {submitted}. You ",
  # "may have included an unnecessary argument, or you ",
  # "may have left out or misspelled an important ",
  # "argument name."
  # intro <- build_intro(.call = enclosing_call, .arg = enclosing_arg)


  # "This code seems correct, but please write with full parameter(s) names."
  # "You wrote {submitted} please rewrite with {solution_name} ."
  # "You wrote {submitted} please rewrite with {solution_name} ."


  submitted_call <- prep(submitted_call)
  submitted <- lapply(submitted, prep) #yes devrait etre quoted
  submitted_user <- submitted

  if (!is.null(submitted_name)) {
    submitted_name <- paste(submitted_name, "= ")
    submitted_user <- purrr::map2(submitted_name, submitted, md_code_prepend)
  }

  if (!is.null(solution_name)) {
    solution_name <- paste(solution_name, "= ")
    solution_name <- purrr::map2(solution_name, submitted, md_code_prepend)
  }

  intro  <- "This code seems correct, but please write using full argument(s) names:\n\n"
  msg <- glue::glue_data(
    list(
      submitted = submitted_user,
      solution_name = solution_name,
      submitted_call = submitted_call
    ),
    "- Where you wrote {submitted}, please use the full formal name {solution_name}."
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
too_many_matches <- function(submitted_call,
                             solution_name,
                             enclosing_call = NULL,
                             enclosing_arg = NULL) {

  # f(1, g(1, h(b = i(1), ba = 2)))
  # f(1, a = g(1, a = h(bab = 1)))

  # "Double check the argument names you are using. ",
  # "In g(1, h(b = i(1), ba = 2)), h() accepts an argument named bab. More than one of your argument names will ",
  # "be matched to bab, which will cause an error. Try ",
  # "writing out the full argument names."

  # "Double check the argument names you are using. ",
  # "{intro}{submitted_call} accepts an argument named {that} and it ",
  # "looks like more than one of your argument names will ",
  # "be matched to {that}, which will cause an error. Try ",
  # "writing out the full argument names."

  submitted_call <- prep(submitted_call)
  solution_name <- prep(solution_name)

  intro <- build_intro(.call = enclosing_call, .arg = enclosing_arg)

  glue::glue_data(
    list(
      intro = intro,
      submitted_call = submitted_call,
      solution_name = solution_name
    ),
    "{intro}{submitted_call} accepts an argument named {solution_name}. ",
    "More than one of your argument names in {submitted_call} will ",
    "be matched to {solution_name}, which will cause an error. Try ",
    "writing out the full argument names."
  )
}

# wrong call
wrong_call <- function(submitted,
                       solution,
                       submitted_name = NULL,
                       enclosing_call = NULL) {

  # f(1, g(1, h(a = i(1))))
  # f(1, a = g(1, a = h(a = j(1))))

  # "g(1, h(i(1))), I expected you to call a = j() where you called a = i()."

  # "{intro}I expected you to {action} {solution} where you called {submitted}."

  intro <- build_intro(.call = enclosing_call)

  solution_original <- solution
  submitted <- prep(submitted)
  solution <- prep(solution)

  if (!is.null(submitted_name) && submitted_name != "") {
    solution <- md_code_prepend(paste(submitted_name, "= "), solution)
    submitted <- md_code_prepend(paste(submitted_name, "= "), submitted)
  }
  
  action <- 
    if (is_infix_assign(solution_original)) {
      "assign something to something else with"
    } else {
      "call"
    }

  glue::glue_data(
    list(
      submitted = submitted,
      solution = solution,
      action = action
    ),
    "{intro}I expected you to {action} {solution} where you called {submitted}."
  )
}

# wrong value for wrong value and wrong call, the enclosing argument is the
# argument that appears before the call or value. It should be passed to
# submitted_name
wrong_value <- function(submitted,
                        solution,
                        submitted_name = NULL,
                        enclosing_call = NULL) {

  # f(1, g(1, h(1)))
  # f(1, a = g(1, a = h(2)))

  # "h(1), I expected 2 where you wrote 1."

  # "{intro}I expected {solution} where you wrote {submitted}."

  intro <- build_intro(.call = enclosing_call)

  expected <- "expected"
  if (length(submitted) > length(solution)) {
    expected <- "didn't expect"
    solution <- submitted
    submitted <- NULL
  }
  
  where <- " where you wrote "
  
  solution_original <- solution
  solution <- prep(solution)
   
  if (is.null(submitted)) {
    intro <- ""
    submitted <- build_intro(enclosing_call %||% solution_original, .open = "", .close = "")
  } else {
    submitted <-prep(submitted)
  }
      
  if (!is.null(submitted_name) && submitted_name != "") {
    solution <- md_code_prepend(paste(submitted_name, "= "), solution)
    submitted <- md_code_prepend(paste(submitted_name, "= "), submitted)
  }

  # NOTE: infix operators that are calls like `<-` also
  # need to be accounted for but perhaps there's a cleaner
  # solution than tacking on more greps.
  action <- 
    if (is_infix_assign(solution_original)) {
      "you to assign something to something else with "
    } else if (grepl("\\(\\)", solution)) {
      "you to call "
    }

  glue::glue_data(
    list(
      intro = intro,
      expected = expected,
      solution = solution,
      where = if (!identical(submitted, "")) where else "",
      submitted = submitted,
      action = action %||% ""
    ),
    "{intro}I {expected} {action}{solution}{where}{submitted}."
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

  if(!is.null(.call)) {
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
