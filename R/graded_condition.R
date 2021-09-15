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
  correct <- if (length(condition_obj$correct)) {
    if (condition_obj$correct) "[Correct]" else "[Incorrect]"
  } else {
    "[Neutral]"
  }
  
  message <- paste(as.character(condition_obj$message), collapse = " ")
  len_message <- nchar(message) + nchar(correct) + 3 + nchar("<gradethis_graded>")
  
  if (len_message > (0.9 * getOption("width")) || grepl("\n", message)) {
    message <- strsplit(message, "\n")
    message <- lapply(message, strwrap, indent = 2, exdent = 2)
    message <- c(unlist(message), "")
    paste(c(correct, message), collapse = "\n")
  } else {
    paste(correct, message, collapse = " ")
  }
}

# Capture and handle errors, by default turning them into internal problem grades.
# Use `fail_if_error()` to convert errors to failing grades.
capture_errors <- function(expr, on_error = NULL) {
  if (is.null(on_error)) {
    on_error <- function(err, that_env) {
      # get relevant calls from the stack to improve error reporting
      calls <- sys_calls_most_helpful()
      
      # Rewrite the error call with the more helpful call
      err$call <- calls$last
      
      if (!is.null(calls$first)) {
        calls$first <- deparse(calls$first, getOption("width", 80))
        err$gradethis_call <- paste(calls$first, collapse= "\n")
        
        # Log the errors locally as messages
        message(paste("#>", calls$first, collapse = "\n"))
      }
      message("Error in ", format(conditionCall(err)), ": ", conditionMessage(err))
      
      # Return an internal grading problem
      grade <- capture_graded(grade_grading_problem(error = err))
      rlang::return_from(that_env, grade)
    }
  }
  stopifnot(is.function(on_error))

  this_env <- rlang::current_env()
  withCallingHandlers(
    error = function(e) {
      on_error(e, this_env)
    },
    expr
  )
}


sys_calls_most_helpful <- function() {
  # borrowing ideas from rstudio/shiny/blob/2360bde1/R/conditions.R#L434-L448
  calls <- sys.calls()
  calls <- calls[-length(calls)] # not this function, obviously
  callnames <- calls_name(calls)
  
  # We want to locate the last call in the stack that isn't just error handling
  # infrastructure. The following functions are part of the stack when an error
  # is captured and surfaced in gradethis, the hope is that the last call before
  # the final block of these functions is reasonably informative.
  hideable <- callnames %in% c(".handleSimpleError", "h", "on_error", "on_graded")
  
  # we also want to find the highest-level gradethis call, which probably
  # provides the full context of the code generating the error
  gradethis_pattern <- "^(gradethis:::?)?(grade_this|grade_result)"
  idx_gradethis <- grep(gradethis_pattern, callnames)
  
  if (!length(idx_gradethis)) {
    # next best guess, the function one level above the first function called
    # in the function returned by grade_this()
    idx_with_code_feedback <- which(callnames == "with_maybe_code_feedback")
    if (length(idx_with_code_feedback)) {
      idx_gradethis <- min(idx_with_code_feedback) - 1
    }
  }
  
  list(
    first = if (length(idx_gradethis)) calls[[min(idx_gradethis)]],
    last = calls[[max(which(!hideable))]]
  )
}

calls_name <- function(calls) {
  # borrowed from rstudio/shiny/blob/2360bde1/R/conditions.R#L64-L76
  sapply(calls, function(call) {
    if (is.function(call[[1]])) {
      "<Anonymous>"
    } else if (inherits(call[[1]], "call")) {
      paste0(format(call[[1]]), collapse = " ")
    } else if (typeof(call[[1]]) == "promise") {
      "<Promise>"
    } else {
      paste0(as.character(call[[1]]), collapse = " ")
    }
  })
}

gradethis_fail_error_handler <- function(message, env = parent.frame(), ...) {
  force(list(message, env, args = list(...)))
  function(err, that_env) {
    # Add condition message as `.error_message` for use in the glue string
    # but use a child env of `env` so we don't trample anything by accident
    env_child <- new.env(parent = env)
    assign(".error", err, envir = env_child)
    assign(".error_message", conditionMessage(err), envir = env_child)
    
    grade <- capture_graded(fail(message, ..., env = env_child, error = err))
    rlang::return_from(that_env, grade)
  }
}

## This function solves the situation of trying to execute a "single line of code" code block
## Because the gradethis condition is "thrown", the code block will return when the first condition is received
## Ex:
# {
#   pass_if_throw(1)
#   pass_if_throw(2)
#   pass_if_throw(3)
# }
capture_graded <- function(expr, on_graded = NULL) {
  if (is.null(on_graded)) {
    on_graded <- function(grade, that_env) {
      rlang::return_from(that_env, grade)
    }
  }
  stopifnot(is.function(on_graded))

  this_env <- rlang::current_env()
  withCallingHandlers(
    gradethis_graded = function(grade) {
      on_graded(grade, this_env)
    },
    expr
  )
}

ignore_graded <- function(expr) {
  capture_graded(
    expr,
    on_graded = function(grade, ignore) {
      # do nothing
    }
  )
}

#' Capture grades and errors
#'
#' 
#' Capture the first [graded()] signal or error thrown when evaluating the
#' `expr`.
#' 
#' @examples
#' # Passes with "message 1", short-circuiting evaluation
#' eval_gradethis({
#'   pass("message 1")
#'   pass("message 2")
#'   pass("message 3")
#' })
#' 
#' # Fails with message from fail()
#' eval_gradethis({
#'   fail("incorrect")
#'   pass("correct")
#' })
#' 
#' # Fails with message from expect_true()
#' eval_gradethis({
#'   testthat::expect_true(FALSE)
#'   pass("message 2")
#'   pass("message 3")
#' })
#' 
#' # Fails immediately with message "boom"
#' eval_gradethis({
#'   stop("boom")
#'   pass("message 2")
#'   pass("message 3")
#' })
#'   
#' @param expr The expression or code block to evaluate
#' @param on_error A [withCallingHandlers()] handler for class `error` with
#'   signature `function(error, this_env)` that receives the error object and
#'   calling environment of the error handler. `on_error` should use
#'   [rlang::return_from()] using `this_env` to immediately return the value and
#'   not continue evaluation.
#' @param on_graded A [withCallingHandlers()] handler for class `graded` with
#'   signature `function(grade, this_env)` that receives the error object and
#'   calling environment of the error handler. `on_graded` should use
#'   [rlang::return_from()] using `this_env` to immediately return the value and
#'   not continue evaluation.
#' 
#' @keywords internal
#' @export
eval_gradethis <- function(expr, on_error = NULL, on_graded = NULL) {
  capture_graded(
    on_graded = on_graded,
    capture_errors(
      on_error = on_error,
      expr
    )
  )
}
