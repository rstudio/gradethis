# given a user and solution expression,
# recursively detect differences
# detect_mistakes_old <- function(user,
#                             solution, env = parent.frame()) {
#   force(env)
# 
#   # code should be checked in the opposite order
#   # of evaluation (e.g. from the outside in for
#   # nested notation), whether or not the student
#   # (and/or teacher) used a pipe
#   user <- rev(order_calls(unpipe_all(user), env = env))
#   solution <- rev(order_calls(unpipe_all(solution), env = env))
#   
#   
#   # do comparing named and unnamed arguments here
#   # probably something using rlang::call_arg_names
#   
#   # user_len > solution_len ## surplus
#   # user_len < solution_len ## missing
# 
#   # then for loop.
# 
#   max_length <- max(length(user), length(solution))
# 
#   for (i in seq_len(max_length)) {
# 
    ## missing/surplus argument should not just compare raw position
#     ## should determine what is missing/surplus by names
#     # Did the user miss something?
#     if (i > length(user)) {
#       return(missing_argument(this_call = user[[i - 1]],
#                               that = solution[[i]],
#                               that_name = names(solution[i])))
#     }
# 
#     # Did the user write too much?
#     if (i > length(solution)) {
#       return(surplus_argument(this_call = user[[i - 1]][1],
#                               this_name = names(user[i]),
#                               this = ifelse(is.call(user[[i]]),
#                                             renest(user[i:length(user)]),
#                                             user[[i]])))
#     }
# 
#     # Does the user code not match the solution code?
#     if (length(user[[i]]) != length(solution[[i]]) ||
#         !identical(user[[i]], solution[[i]])
#         ) {
#       return(isolate_mismatch(user, solution, i))
#     }
#   }
#   NULL
# }

detect_mistakes <- function(user, 
                            solution, 
                            env = rlang::env_parent(), 
                            enclosing_call = NULL,
                            enclosing_arg = NULL) {
  force(env)
  # rlang::env_print(env)
  # print(sort(rlang::env_names(env)))

  if (is.call(user)) {
    user <- call_standardise_formals(unpipe_all(user), env = env)
  }
  if (is.call(solution)) {
    solution <- call_standardise_formals(unpipe_all(solution), env = env)
  }

  # if the code is not a call, it 
  # should be a value identical to the solution
  # BUT WHAT IF ONE IS A CALL THAT EVALUATES TO THE VALUE OF THE OTHER?
  if (!is.call(user) || !is.call(solution)) {
    if (!identical(user, solution)) {
      return(
        wrong_value(
          this = deparse_to_string(user),
          that = solution
        )
      )
    }
  }
  # We can assume anything below here is a call

  user_names <- rlang::names2(user)
  solution_names <- rlang::names2(solution)
  
  # Dividing cases into groups based on the relative lengths of the user's code
  # and the solution code produces unitelligible messages as in issue #84. To
  # produce more transparent messages that accord with how a user thinks of calls,
  # check these things in this order:
  
  # 1. Check that the user and the solution use the same call
  # SHOULD WE HAVE A TARGETED WRONG CALL FUNCTION?
  if (!identical(user[[1]], solution[[1]])) {
    return(
      wrong_call(
        this = user,
        that = solution,
        enclosing_call = enclosing_call,
        enclosing_arg = enclosing_arg
      )
    )
  }
  
  # 2. Check that every named argument in the solution appears in the user code.
  #    The outcome of this order is that when a user writes na = TRUE, gradethis
  #    will tell them that it expected an na.rm argument, not that na is a surplus
  #    argument.
  actual_solution_names <-  solution_names[solution_names != ""]
  missing_args <- actual_solution_names[!(actual_solution_names %in% user_names)]
  if (length(missing_args)) {
    missing_name <- missing_args[1]
    return(
      missing_argument(
        this_call = solution,
        that = solution[[missing_name]],
        that_name = missing_name
      )
    )
  }
  
  # 3. Check that the user code does not contain any named arguments that do not
  #    appear in the solution code. Since both calls have been standardised, these
  #    named arguments can only be being passed to ... and we should not match by
  #    position a named argument that is passed to ... with an unamed argument
  #    passed to ...
  unmatched_user_names <- user_names[!(user_names %in% solution_names)] 
  unmatched_user_names <- unmatched_user_names[unmatched_user_names != ""]
  if (length(unmatched_user_names)) {
    surplus_name <- unmatched_user_names[1]
    return(
      surplus_argument(
        this_call = user,
        this = user[[surplus_name]],
        this_name = surplus_name
      )
    )
  }
  
  # 4. Check that every named argument in the solution matches every
  #    correspondingly named argument in the user code.
  for (name in actual_solution_names) {
    if (!identical(user[[name]], solution[[name]])) {
      return(
        detect_mistakes(
          user = user[[name]], 
          solution = solution[[name]], 
          env = env,
          # if below is too verbose, consider user[1] 
          enclosing_call = deparse_to_string(user),
          enclosing_arg = name
        )
      )
    }
  }
  
  
  # 5. Extract the remaining arguments from the user code and the solution code.
  #    Pair them in the order that they occur, checking that each pair matches.
  #    Check pairs in sequence and address unmatched arguments when you get to
  #    them.
  user_args <- user[-1]         # remove the call
  solution_args <- solution[-1] # remove the call
  
  for (name in actual_solution_names) {
    user_args[[name]] <- NULL
    solution_args[[name]] <- NULL
  }
  
  user_len <- length(user_args)
  solution_len <- length(solution_args)

  n <- max(user_len, solution_len)
  
  for (i in seq_len(n)) {
    
    # if solution argument is unmatched due to no remaining user arguments
    if (i > user_len) {
      return(
        missing_argument(
          this_call = solution,
          that = solution_args[[i]],
          that_name = names(solution_args[i])
        )
      )
      
    # if user argument is unmatched due to no remaining solution arguments
    } else if (i > solution_len) {
      return(
        surplus_argument(
          this_call = user,
          this = user_args[[i]],
          this_name = names(user_args[i])
        )
      )
      
    # The user argument has a matching solution argument, are they identical?
    } else {
      if (!identical(user_args[[i]], solution_args[[i]])) {
        return(
          detect_mistakes(
            user = user_args[[i]], 
            solution = solution_args[[i]], 
            env = env,
            # if below is too verbose, consider user[1] 
            enclosing_call = deparse_to_string(user), 
            enclosing_arg = name
          )
        )
      }
    }
  }

  # No missmatch found
  return(NULL)
}

# isolate_mismatch_2 <- function(user, solution, i) {
#   # For a given order call, we found code that is different
#   # if the the first argument is different,
#   # i.e., the wrong function was used,  mean(1:3) vs std(1:3)
#   # return wrong message
#   if (length(user[[i]]) == 1 &&
#       length(solution[[i]]) == 1) {
# 
#     wrong <- prep_snippet(user, i)
#     right <- prep_snippet(solution, i, .solution = TRUE)
# 
#     return(wrong_value(this = wrong,
#                        that = right,
#                        this_name = names(user[i]),
#                        that_name = names(solution[i])))
# 
#     # If we cannot do this, we are working with two
#     # multipart calls and we need to identify which
#     # elements of the calls do not align (here we
#     # rely heavily on the fact that both calls have
#     # been previously standardized)
#   }
# 
#   # some other part of the code is different
#   detect_mistakes(user[[i]], solution[[i]])
# }

# isolate_mismatch <- function(user, solution, i) {

#   # We've honed in on the error when we can narrow
#   # it down to a single incorrect user element
#   # matched to a single correct solution element
#   if (length(user[[i]]) == 1 &&
#       length(solution[[i]]) == 1) {

#     wrong <- prep_snippet(user, i)
#     right <- prep_snippet(solution, i, .solution = TRUE)

#     return(wrong_value(this = wrong,
#                        that = right,
#                        this_name = names(user[i]),
#                        that_name = names(solution[i])))

#     # If we cannot do this, we are working with two
#     # multipart calls and we need to identify which
#     # elements of the calls do not align (here we
#     # rely heavily on the fact that both calls have
#     # been previously standardized)
#   } else {

#     user_call <- user[[i]]
#     solution_call <- solution[[i]]

#     # First check that the calls match.
#     if (user_call[[1]] != solution_call[[1]]) {
#       wrong <- renest(user[i:length(user)])
#       right <- ifelse(is_infix(solution_call[1]),
#                       renest(solution[i:length(solution)]),
#                       solution_call[1])

#       return(wrong_value(this = wrong,
#                          that = right,
#                          this_name = names(user[i]),
#                          that_name = names(solution[i])))
#     }

#     ## 

#     # Then inspect the first argument (which appears on the next line)
#     if (length(user) == i && length(solution) > i) {
#       return(missing_argument(this_call = user[[i]],
#                               that = solution[[i + 1]],
#                               that_name = names(solution[i + 1])))

#     } else if (length(user) > i && length(solution) == i) {
#       return(surplus_argument(this_call = user[[i]][1],
#                               this_name = names(user[i + 1]),
#                               this = ifelse(is.call(user[[i + 1]]),
#                                             renest(user[(i + 1):length(user)]),
#                                             user[[i + 1]])))

#     } else if (length(user) > i && length(solution) > i) {
#       if (!names_match(user, solution, i + 1)) {
#         return(wrong_value(this = user[[i + 1]],
#                            this_name = names(user[i + 1]),
#                            that = solution[[i + 1]],
#                            that_name = names(solution[i + 1])))

#       } else if (user[[i + 1]] != solution[[i + 1]]) {
#         if (is_infix(user_call[1])) {
#           return(detect_mistakes(user_call, solution_call))
#         } else {
#           return(detect_mistakes(renest(user[(i + 1):length(user)]),
#                                     renest(solution[(i + 1):length(solution)])))
#         }
#       }
#     }

#     # Then iterate through the remaining arguments one at a time
#     # here we are iterating across a call instead of down a call
#     # stack as in detect_mistakes

#     max_length <- max(length(user_call), length(solution_call))

#     for (j in seq_len(max_length)) {
#       if (j == 1) next

#       # Did the user leave out an argument?
#       if (j > length(user_call)) {
#         return(missing_argument(this_call = user_call[1],
#                                 that = solution_call[[j]],
#                                 that_name = names(solution_call[j])))
#       }

#       # Did the user include an extra argument?
#       if (j > length(solution_call)) {
#         return(surplus_argument(this_call = user_call[1],
#                                 this = user_call[[j]],
#                                 this_name = names(user_call[j])))
#       }

#       # Do the argument names match?
#       if (!names_match(user_call, solution_call, j)) {
#         return(wrong_value(this = user_call[[j]],
#                            this_name = names(user_call[j]),
#                            that = solution_call[[j]],
#                            that_name = names(solution_call[j])))
#       }

#       # Do two arguments conflict? They may themselves
#       # contain an expression that we should drill into.
#       if (user_call[[j]] != solution_call[[j]]) {
#         return(detect_mistakes(user_call, solution_call))
#       }
#     }
#   }
#   NULL
# }

real_name <- function(name) {
 !is.null(name) && name != ""
}

names_match <- function(user, solution, i) {

  if (is.null(names(user))) return(TRUE)
  user_names <- names(user)

  if (is.null(names(solution))) return(TRUE)
  solution_names <- names(solution)

  if (real_name(user_names[i])) {
    if (real_name(solution_names[i])) {
      if (user_names[i] != solution_names[i]) {
        return(FALSE)
      }
    } else if (user_names[i] %in% solution_names) {
      return(FALSE)
    }
  } else if (real_name(solution_names[i]) &&
             solution_names[i] %in% user_names) {
    return(FALSE)
  }
  TRUE
}

prep_snippet <- function(code, i, .solution = FALSE) {

  # errors that involve an infix operator make more
  # sense if the explanation refers to the operator
  if (i == 2 && is_infix(code[[1]]) && length(code[[1]]) == 1) {
    paste(deparse_to_string(code[[1]][[1]]), deparse_to_string(code[[2]]))

    # Return the internal arguments of user code, but
    # not solution code (that could give away too much)
  } else if (is.call(code[[i]])) {
    ifelse(.solution, code[[i]][1], renest(code[i:length(code)]))
  } else {
    code[[i]]
  }

}
