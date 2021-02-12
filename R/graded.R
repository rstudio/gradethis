#' Signal a final grade for a student's submission
#'
#' `graded()` is used to signal a final grade for a submission. In general, it
#' is most helpful when used in [grade_this()], but it is used internally by
#' other grading functions to signal that the student's submission has been
#' graded. If you're using [grade_this()], you'll most likely want to use the
#' helper functions `pass()`, `fail()`, `pass_if_equal()`, and
#' `fail_if_equal()`, rather than to call `graded()` directly.
#'
#' @section Usage in `gradethis_exercise_checker()`:
#'
#'   If \pkg{gradethis} is used in a [learnr::tutorial()] with the default
#'   [gradethis_setup()], [gradethis_exercise_checker()] expects the `*-check`
#'   chunk for an exercise to return a function. When the exercise submission is
#'   to be graded, [gradethis_exercise_checker()] will call the checking
#'   function, providing it with a consistent exercise submission environment —
#'   see [mock_this_exercise()] for examples of this environment. The goal of
#'   this function is to evaluate the submission and to return a final grade via
#'   `graded()`.
#'
#'   In general, tutorial authors will primarily use `graded()` and its helper
#'   functions only when using [grade_this()]. Whenever one of these functions
#'   is called inside [grade_this()], the submission checking will stop
#'   immediately and the appropriate grade and feedback will be returned.
#'
#'   Internally, [grade_this_code()], [grade_result()], and [grade_code()] all
#'   create `graded()` objects, but each provides a different mechanism for
#'   grading the submission and tutorial authors do not need to call `graded()`
#'   or its helper functions within those grading functions.
#'   
#' @examples
#' # Suppose our exercise asks the student to prepare and execute code that
#' # returns the value `42`. We'll use `grade_this()` to check their
#' # submission.
#' #
#' # Because we are demonstrating these functions inside R documentation, we'll
#' # save the function returned by `grade_this()` as `this_grader()`. Calling
#' # `this_grader()` on a mock exercise submission is equivalent to running the
#' # check code when the student clicks "Submit Answer" in a learnr tutorial.
#' 
#' this_grader <- 
#' # ```{r example-check}
#'   grade_this({
#'     # Automatically use .result to compare to an expected value
#'     pass_if_equal(42, "Great work!")
#'     
#'     # Similarly compare .result to an expected wrong value
#'     fail_if_equal(41, "You were so close!")
#'     fail_if_equal(43, "Oops, a little high there!")
#'     
#'     # or automatically pass if .result is equal to .solution
#'     pass_if_equal(message = "Great work!")
#'     
#'     # Be explicit if you need to round to avoid numerical accuracy issues
#'     pass_if_equal(x = round(.result), y = 42, "Close enough!")
#'     fail_if_equal(x = round(.result), y = 64, "Hmm, that's not right.")
#'     
#'     # For more complicated calculations, call pass() or fail()
#'     if (.result > 100) {
#'       fail("{.result} is way too high!")
#'     }
#'     if (.result * 100 == .solution) {
#'       pass("Right answer, but {.result} is two orders of magnitude too small.")
#'     }
#'     
#'     # Choose a default grade if none of the above have resulted in a grade
#'     fail()
#'   })
#' # ```
#' 
#' # Now lets try with a few different student submissions ----
#' 
#' # Correct!
#' this_grader(mock_this_exercise(.user_code = 42))
#' 
#' # These were close...
#' this_grader(mock_this_exercise(.user_code = 41))
#' this_grader(mock_this_exercise(.user_code = 43))
#' 
#' # Automatically use .solution if you have a *-solution chunk...
#' this_grader(mock_this_exercise(.user_code = 42, .solution_code = 42))
#' 
#' # Floating point arithmetic is tricky...
#' this_grader(mock_this_exercise(.user_code = 42.000001, .solution_code = 42))
#' this_grader(mock_this_exercise(.user_code = 64.123456, .solution_code = 42))
#' 
#' # Complicated checking situations...
#' this_grader(mock_this_exercise(.user_code = 101, .solution_code = 42))
#' this_grader(mock_this_exercise(.user_code = 0.42, .solution_code = 42))
#' 
#' # Finally fall back to the final answer...
#' this_grader(mock_this_exercise(.user_code = 33, .solution_code = 42))
#'
#' @param message A character string of the message to be displayed.
#' @param correct A logical value of whether or not the checked code is correct.
#' @param x First item in the comparison. By default, when used inside
#'   [grade_this()], `x` is automatically assigned the value of `.result` — in
#'   other words the result of running the student's submitted code. `x` is not
#'   the first argument since you will often want to compare the final value of
#'   the student's submission against a specific value, `y`.
#' @param y The expected value against which `x` is compared using
#'   `waldo::compare(x, y)`. In `pass_if_equal()`, if no value is provided, the
#'   exercise `.solution`, or the result of evaluating the code in the
#'   exercise's `*-solution` chunk, will be used for the comparison.
#' @param ... Additional arguments passed to `graded()` or otherwise ignored
#' @param type,location The `type` and `location` of the feedback object
#'   provided to \pkg{learnr}. See
#'   <https://rstudio.github.io/learnr/exercises.html#Custom_checking> for more
#'   details.
#'
#'   `type` may be one of "auto", "success", "info", "warning", "error", or
#'   "custom".
#'
#'   `location` may be one of "append", "prepend", or "replace".
#' 
#' @return `pass()` and `pass_if_equal()` signal a _correct_ grade with a
#'   glue-able `message`.
#'
#'   `fail()` and `fail_if_equal()` signal an _incorrect_ grade with a
#'   glue-able `message`.
#'   
#'   `graded()` signals a correct or incorrect grade according to the logical
#'   value of `correct`, with a standard character (unglued) `message`.
#' 
#' @describeIn graded Prepare and signal a graded result.
#' @export
graded <- function(correct, message = NULL, ..., type = NULL, location = NULL) {
  ellipsis::check_dots_empty()
  
  # allow logical(0) to signal a neutral grade
  checkmate::expect_logical(correct, any.missing = FALSE, max.len = 1, null.ok = FALSE)

  obj <- structure(
    list(
      message = message %||% "",
      correct = correct,
      type = type,
      location = location
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


#' @describeIn graded Signal a _passing_ grade.
#' @param env environment to evaluate the glue `message`. Most users of
#'   \pkg{gradethis} will not need to use this argument.
#' @export
pass <- function(
  message = getOption("gradethis.pass", "Correct!"),
  ...,
  env = parent.frame()
) {
  graded(message = glue_message_with_env(env, message), correct = TRUE, ...)
}

#' @describeIn graded Signal a _failing_ grade.
#' @export
fail <- function(
  message = getOption("gradethis.fail", "Incorrect"),
  ...,
  env = parent.frame()
) {
  graded(message = glue_message_with_env(env, message), correct = FALSE, ...)
}


#' @describeIn graded Signal a _passing_ grade only if `x` and `y` are equal.
#' 
#' @export
pass_if_equal <- function(
  y = rlang::missing_arg(),
  message = getOption("gradethis.pass", "Correct!"),
  x = rlang::missing_arg(),
  ...,
  env = parent.frame()
) {
  if (rlang::is_missing(x)) {
    x <- get_from_env(".result", env)
    if (rlang::is_missing(x)) {
      return(missing_object_in_env(".result", env, "pass_if_equal"))
    }
  }
  if (rlang::is_missing(y)) {
    y <- get_from_env(".solution", env)
    if (rlang::is_missing(y)) {
      return(missing_object_in_env(".solution", env, "pass_if_equal"))
    }
  }
  grade_if_equal(x = x, y = y, message = message, correct = TRUE, env = env, ...)
}

#' @describeIn graded Signal a _failing_ grade only if `x` and `y` are equal.
#' @export
fail_if_equal <- function(
  y,
  message = getOption("gradethis.fail", "Incorrect"),
  x = rlang::missing_arg(),
  ...,
  env = parent.frame()
) {
  if (rlang::is_missing(x)) {
    x <- get_from_env(".result", env)
    if (rlang::is_missing(x)) {
      return(missing_object_in_env(".result", env, "fail_if_equal"))
    }
  }
  grade_if_equal(x = x, y = y, message = message, correct = FALSE, env = env, ...)
}

grade_if_equal <- function(x, y, message, correct, env, ...) {
  compare_msg <- waldo::compare(x, y)
  if (length(compare_msg) > 0) {
    # not equal! quit early
    return()
  }

  # equal!
  graded(message = glue_message_with_env(env, message), correct = correct, ...)
}


legacy_graded <- function(...) {
  capture_graded(
    graded(...)
  )
}

get_from_env <- function(x, env) {
  get0(x, envir = env, ifnotfound = rlang::missing_arg())
}

missing_object_in_env <- function(obj, env, caller) {
  label <- get0(".label", env, ifnotfound = NULL)
  label <- if (!is.null(label)) paste0("In exercise `", label, "`: ")
  message(
    label,
    "`", caller, "()`: expected `", obj, "` to be found", 
    " in its calling environment or the environment specified by `env`.",
    " Did you call `", caller, "()`",
    " inside `grade_this()` or `grade_this_code()`?"
  )
  # Signal problem with grading code
  graded(FALSE, feedback_grading_problem()$message)
}
