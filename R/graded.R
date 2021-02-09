#' Graded object for submission value
#'
#' The return value from `graded` should be returned by every
#' `*-check` chunk when used with [grade_learnr()].
#'
#' `graded()` objects are signaled to the calling functions.
#'
#'   * [grade_result()] ignores when grades are created. `graded()` objects must be returned
#'   * [grade_code()] handles `graded()` objects internally
#'   * [grade_this()] will stop execution once a `pass()`, `pass_if_equal()`,
#'     `fail()`, `fail_if_equal()`, or `grade()` is called. To generate programmatic grades,
#'     use `graded()` or if statements around `pass*()` and `fail*()`
#'
#' @param message A character string of the message to be displayed.
#' @param correct A logical value of whether or not the checked code is correct.
#' 
#' @describeIn graded Programmatic function to produce a graded a result.
#' @export
graded <- function(correct, message = NULL) {
  checkmate::expect_logical(correct, any.missing = FALSE, len = 1, null.ok = FALSE)

  obj <- structure(
    list(
      message = message %||% "",
      correct = correct
    ),
    class = c("gradethis_graded", "condition")
  )

  # Signal to parent function calls that a grade has been made
  signalCondition(obj)

  # return the object
  obj
}

is_graded <- function(x) {
  inherits(x, "gradethis_graded")
}


#' @describeIn graded Produce a _passing_ grade
#' @param env environment to evaluate the glue `message`
#' @export
pass <- function(message = getOption("gradethis.pass", "Correct!"), env = parent.frame()) {
  graded(message = glue_message_with_env(env, message), correct = TRUE)
}

#' @describeIn graded Produce a _failing_ grade
#' @export
fail <- function(message = getOption("gradethis.fail", "Incorrect"), env = parent.frame()) {
  graded(message = glue_message_with_env(env, message), correct = FALSE)
}


#' @describeIn graded Produce a _passing_ grade only if [waldo::compare()]
#'   passes on `x` and `y`
#' @param x First item in the comparison. By default, when used inside
#'   [grade_this()], this is the `.result` object, or the result of running the
#'   student's submitted code. Generally, you will not need to supply a value
#'   for this argument, so it is not the first argument.
#' @param y The expected value against which `x` is compared using 
#'   `waldo::compare(x, y)`. In `pass_if_equal()`, if no value is provided, the
#'   exercise `.solution`, or the result of evaluating the code in the
#'   exercise's `*-solution` chunk, will be used for the comparison.
#'   
#' @return A correct graded condition with the glue-ed `message`
#' 
#' @export
pass_if_equal <- function(
  y = rlang::missing_arg(),
  message = getOption("gradethis.pass", "Correct!"),
  x = rlang::missing_arg(),
  env = parent.frame()
) {
  if (rlang::is_missing(x)) {
    x <- get_from_env(".result", env, "pass_if_equal")
    if (is_graded(x)) return(x)
  }
  if (rlang::is_missing(y)) {
    y <- get_from_env(".solution", env, "pass_if_equal")
    if (is_graded(y)) return(y)
  }
  grade_if_equal(x = x, y = y, message = message, correct = TRUE, env = env)
}

#' @describeIn graded Produce a _failing_ grade only if [testthat::compare()]
#'   passes on `x` and `y`
#'
#' @return An incorrect graded condition with the glue-ed `message`
#' @export
fail_if_equal <- function(
  y,
  message = getOption("gradethis.fail", "Incorrect"),
  x = rlang::missing_arg(),
  env = parent.frame()
) {
  if (rlang::is_missing(x)) {
    x <- get_from_env(".result", env, "fail_if_equal")
    if (is_graded(x)) return(x)
  }
  grade_if_equal(x = x, y = y, message = message, correct = FALSE, env = env)
}

grade_if_equal <- function(x, y, message, correct, env) {
  compare_msg <- waldo::compare(x, y)
  if (length(compare_msg) > 0) {
    # not equal! quit early
    return()
  }

  # equal!
  graded(message = glue_message_with_env(env, message), correct = correct)
}


legacy_graded <- function(...) {
  capture_graded(
    graded(...)
  )
}

get_from_env <- function(x, env, .caller) {
  tryCatch(
    get(x, envir = env),
    error = function(e) {
      label <- get0(".label", env, ifnotfound = NULL)
      label <- if (!is.null(label)) paste0("In exercise `", label, "`: ")
      not_found <- sprintf(
        gettext("object %s not found", domain = "R"), 
        paste0("'", x, "'")
      )
      if (grepl(not_found, e$message, fixed = TRUE)) {
        message(
          label,
          "`", .caller, "()`: expected `", x, "` to be found in its calling environment",
          " or the environment specified by `env`. Did you call `", .caller, "()`",
          " inside `grade_this()` or `grade_this_code()`?"
        )
      } else {
        message(label, e$message)
      }
      # Signal problem with grading code
      graded(FALSE, feedback_grading_problem()$message)
    }
  )
}
