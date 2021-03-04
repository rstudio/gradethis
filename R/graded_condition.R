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
  
  if (len_message > (0.9 * getOption("width"))) {
    message <- strsplit(message, "\n")
    message <- lapply(message, strwrap, indent = 2, exdent = 2)
    message <- c(unlist(message), "")
    paste(c(correct, message), collapse = "\n")
  } else {
    paste(correct, message, collapse = " ")
  }
}

# Turn errors into `fail()`ures
capture_errors <- function(expr, on_error = NULL) {
  if (is.null(on_error)) {
    on_error <- function(e, that_env) {
      # must wrap in ignore statement to retrieve fail object
      ret <- capture_graded({
        fail(conditionMessage(e))
      })
      rlang::return_from(that_env, ret)
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

# helper method to evaluate an expr
# will capture errors and turn them into `failure("message")`
#

#' Capture grades and errors
#'
#' Capture the first [graded()] signal or error thrown when evaluating the
#' `expr`.
#' 
#' @param expr Code block to evaluate
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
#' @examples
#' ans1 <- eval_gradethis({
#'   pass("message 1")
#'   pass("message 2")
#'   pass("message 3")
#' })
#' ans1 # passing - message 1
#'
#' ans2 <- eval_gradethis({
#'   testthat::expect_true(FALSE)
#'   pass("message 2")
#'   pass("message 3")
#' })
#' ans2 # failing - FALSE isn't true.
#'
#' ans3 <- eval_gradethis({
#'   stop("boom")
#'   pass("message 2")
#'   pass("message 3")
#' })
#' ans3 # failing - boom
#' 
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
