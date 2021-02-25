#' Signal a final grade for a student's submission
#'
#' `graded()` is used to signal a final grade for a submission. Most likely,
#' you'll want to use its helper functions: `pass()`, `fail()`, 
#' `pass_if_equal()`, `fail_if_equal()`, `pass_if()` and `fail_if()`. When used
#' in [grade_this()], these functions signal a final grade and no further
#' checking of the student's submitted code is performed.
#' 
#' @section Return a grade immediately:
#' 
#'   `graded()` and its helper functions are designed to short-circuit further
#'   evaluation whenever they are called. If you're familiar with writing
#'   functions in R, you can think of `graded()` (and `pass()`, `fail()`, etc.)
#'   as a special version of `return()`.
#'   
#'   The early return behavior can be helpful when you have to perform 
#'   complicated or long-running tests to determine if a student's code 
#'   submission is correct. We recommend that you perform the easiest tests
#'   first, progressing to the most complicated tests. By taking advantage of
#'   early grade returns, you can simplify your checking code:
#'   
#'   ````
#'   ```{r}
#'   grade_this({
#'     # is the answer a tibble?
#'     if (!inherits(.result, "tibble")) {
#'       fail("Your answer should be a tibble.")
#'     }
#'     
#'     # from now on we know that .result is a tibble...
#'     if (nrow(.result) == 5) {
#'       fail("Your table should have 5 rows")
#'     }
#'     
#'     # ...and it has 5 rows
#'     if (.result[[1]][[5]] != 5) {
#'       fail("The value of the 5th row of the 1st column should be 5.")
#'     }
#'     
#'     # all of the above checks have passed now.
#'     pass()
#'   })
#'   ```
#'   ````
#'   
#'   Notice that it's important to choose a final fall-back grade as the last
#'   value in your [grade_this()] checking code. This last value is the default
#'   grade that will be given if the submission passes all other checks. If
#'   you're using the standard [gradethis_setup()] and you call `pass()` or 
#'   `fail()` without arguments, `pass()` will return a random praising phrase
#'   and `fail()` will return code feedback (if possible) with an encouraging
#'   phrase.
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
#'   In general, tutorial authors should only use `graded()` and its helper
#'   functions within [grade_this()]. Whenever one of these functions is called
#'   inside [grade_this()], the submission checking will stop immediately and
#'   the appropriate grade and feedback will be returned.
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
#' @param ... Additional arguments passed to `graded()` or otherwise ignored.
#'   Ignored by `pass_if()` and `fail_if()`.
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
#' @return 
#'   - `pass()` and `pass_if_equal()` signal a _correct_ grade with a
#'     glue-able `message`.
#'
#'   - `fail()` and `fail_if_equal()` signal an _incorrect_ grade with a
#'     glue-able `message`.
#'   
#'   - `pass_if()` and `fail_if()` signal a correct or incorrect grade if the
#'     provided condition is `TRUE`, with a glue-able `message`.
#'   
#'   - `graded()` signals a correct or incorrect grade according to the logical
#'     value of `correct`, with a standard character (unglued) `message`.
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

signal_grade <- function(grade, env = parent.frame()) {
  signalCondition(grade)
  rlang::return_from(env, grade)
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
#' @param hint Include a code feedback hint with the failing message? This
#'   argument only applies to `fail()` and `fail_if_equal()` and the message is
#'   added using the default options of [give_code_feedback()] and
#'   [maybe_code_feedback()]. The default value of `hint` can be set using
#'   [gradethis_setup()] or the `gradethis.fail.hint` option.
#' @export
fail <- function(
  message = getOption("gradethis.fail", "Incorrect"),
  ...,
  env = parent.frame(),
  hint = getOption("gradethis.fail.hint", FALSE)
) {
  maybe_hint(
    hint, 
    env = env, 
    graded(message = glue_message_with_env(env, message), correct = FALSE, ...)
  )
}


#' @describeIn graded Signal a _passing_ grade only if `x` and `y` are equal.
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
  env = parent.frame(),
  hint = getOption("gradethis.fail.hint", FALSE)
) {
  if (rlang::is_missing(x)) {
    x <- get_from_env(".result", env)
    if (rlang::is_missing(x)) {
      return(missing_object_in_env(".result", env, "fail_if_equal"))
    }
  }
  maybe_hint(
    hint, 
    env = env, 
    grade_if_equal(x = x, y = y, message = message, correct = FALSE, env = env, ...)
  )
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


#' @describeIn graded Pass if `cond` is `TRUE`.
#' @param cond For `pass_if()` and `fail_if()`: A logical value or an expression
#'   that will evaluate to a `TRUE` or `FALSE` value. If the value is `TRUE`, or
#'   would be considered `TRUE` in an `if (cond)` statement, then a passing or
#'   failing grade is returned to the user.
#' @export
pass_if <- function(
  cond, 
  message = NULL, 
  ..., 
  env = parent.frame(), 
  x = deprecated()
) {
  ellipsis::check_dots_empty()
  
  if (is_present(x)) {
    deprecate_warn(
      "0.2.3",
      "pass_if(x = )",
      "pass_if(cond = )"
    )
    if (missing(cond)) {
      cond <- x
    }
  }
  
  if (detect_grade_this(env)) {
    assert_gradethis_condition_type_is_value(cond, "pass_if")
    if (cond) {
      message <- message %||% getOption("gradethis.pass", "Correct!")
      pass(message, env = env)
    }
  } else {
    condition(cond, message, correct = TRUE)
  }
}

#' @describeIn graded Fail if `cond` is `TRUE`.
#' @export
fail_if <- function(
  cond, 
  message = NULL, 
  ..., 
  env = parent.frame(),
  hint = getOption("gradethis.fail.hint", FALSE),
  x = deprecated()
) {
  ellipsis::check_dots_empty()
  
  if (is_present(x)) {
    deprecate_warn(
      "0.2.3",
      "fail_if(x = )",
      "fail_if(cond = )"
    )
    if (missing(cond)) {
      cond <- x
    }
  }

  if (detect_grade_this(env)) {
    assert_gradethis_condition_type_is_value(cond, "fail_if")
    if (cond) {
      message <- message %||% getOption("gradethis.fail", "Inorrect.")
      maybe_hint(hint, env = env, fail(message, env = env))
    }
  } else {
    if (!missing(hint) || isTRUE(hint)) {
      warning(
        "The `hint` argument only works when `fail_if()` is called inside `grade_this()`.",
        immediate. = TRUE
      )
    }
    condition(cond, message, correct = FALSE)
  }
}

assert_gradethis_condition_type_is_value <- function(x, from = NULL) {
  type <- condition_type(x)
  if (!identical(type, "value")) {
    from <- if (!is.null(from)) paste0(from, "() ") else ""
    warning(
      from, "does not accept functions or formulas when used inside grade_this().",
      immediate. = TRUE, 
      call. = !is.null(from)
    )
    graded(logical(), feedback_grading_problem()$message, type = "warning")
  }
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

maybe_hint <- function(should_hint, expr, env) {
  grade <- capture_graded(expr)
  if (isTRUE(should_hint)) {
    # we already have the grade, so use the internal s3 method to give feedback
    give_code_feedback_(grade, env = env)
  } else {
    signal_grade(grade)
  }
}
