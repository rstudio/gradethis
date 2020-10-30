## IF some day that `rlang::return_from()` is removed, the code can be replaced with this tryCatch
## However,
##  * The callstack is not as useful when debugging
##  * Code is not allowed to continue from where the original condition was thrown
##    * (As of writing this comment, this is not needed)


## (if the condition makes it to the top level...)
## print method to override a gradethis_graded condition to have a "friendly" print method
#' @export
conditionMessage.gradethis_graded <- function(c) {
  condition_obj <- c

  if (condition_obj$correct) {
    paste0("gradethis - Correct: ", as.character(condition_obj$message))
  } else {
    paste0("gradethis - Incorrect: ", as.character(condition_obj$message))
  }
}

# Turn errors into `fail()`ures
capture_errors <- function(expr, error = error) {
  if (is.null(error)) {
    error <- function(e, that_env) {
      print("turning error into failure")
      str(e)
      ret <- fail(e$message)
      rlang::return_from(that_env)
    }
  }
  this_env <- rlang::current_env()
  withCallingHandlers(
    error = function(e) {
      error(e, this_env)
    },
    expr
  )
}

## This function solves the situation of trying to execute a "single line of code" code block
## Because the gradethis condition is "thrown", the code block will return when the first condition is received
## Ex:
# {
#   pass_if_throw(1)
#   pass_if_throw(2)
#   pass_if_throw(3)
# }
capture_gradethis_conditions <- function(expr) {
  this_env <- rlang::current_env()
  withCallingHandlers(
    gradethis_condition = function(gc_obj) {
      rlang::return_from(this_env, gc_obj)
    },
    expr
  )

  # tryCatch(expr,
  #   # return object right away
  #   gradethis_condition = function(gc_obj) {
  #     gc_obj
  #   }
  # )
}
ignore_gradethis_conditions <- function(expr) {
  withCallingHandlers(
    gradethis_condition = function(gc_obj) {
      # do nothing
    },
    expr
  )
}

# helper method to evaluate an expr
# will capture errors and turn them into `failure("message")`
#
#' @export
eval_gradethis_expr <- function(expr, error = NULL) {
  capture_gradethis_conditions({
    capture_errors(expr, error = error)
  })
}
