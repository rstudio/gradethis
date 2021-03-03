#' Signal a final grade for a student's submission
#'
#' `graded()` is used to signal a final grade for a submission. Most likely,
#' you'll want to use its helper functions: `pass()`, `fail()`,
#' `pass_if_equal()`, `fail_if_equal()`, `pass_if()` and `fail_if()`. When used
#' in [grade_this()], these functions signal a final grade and no further
#' checking of the student's submitted code is performed. See the sections below
#' for more details about how these functions are used in [grade_this()].
#'
#' @section Usage in `grade_this()`:
#'
#'   The `graded()` helper functions are all designed to be called from within
#'   `grade_this()`, but this has the unfortunate side-effect of making their
#'   default arguments somewhat opaque.
#'
#'   The helper functions follow these common patterns:
#'
#'   1. If you don't provide a custom `message`, the default pass or fail
#'      messages will be used. With the default \pkg{gradethis} setup, the pass
#'      message follows the pattern ``r gradethis_default_options$pass`` , and
#'      the fail message follows ``r gradethis_default_options$fail``.
#'
#'      You can set the default message pattern using the `pass` and `fail` in
#'      [gradethis_setup()], or the options `gradethis.pass` and
#'      `gradethis.fail`.
#'
#'      In the custom `message`, you can use [glue::glue()] syntax to reference
#'      any of the available variables in [grade_this()] or that you've created
#'      in your checking code: e.g. `"Your table has {nrow(.result)} rows."`.
#'
#'   2. `pass_if_equal()` and `fail_if_equal()` automatically compare their
#'      first argument against the `.result` of running the student's code.
#'      `pass_if_equal()` takes this one step further and if called without any
#'      arguments will compare the `.result` to the value returned by evaluating
#'      the `.solution` code, if available.
#'
#'   3. All `fail` helper functions have an additional `hint` parameter. If
#'      `hint = TRUE`, a code feedback hint is added to the custom `message`.
#'      You can also control `hint` globally with [gradethis_setup()].
#'
#'   4. All helper functions include an `env` parameters. You can generally
#'      ignore this argument. It's used internally to help `pass()` and `fail()`
#'      _et al._ find the default argument values and to build the `message`
#'      using [glue::glue()].
#'
#' @section Return a grade immediately:
#'
#'   `graded()` and its helper functions are designed to short-circuit further
#'   evaluation whenever they are called. If you're familiar with writing
#'   functions in R, you can think of `graded()` (and `pass()`, `fail()`, etc.)
#'   as a special version of `return()`. If a grade is created, it is returned
#'   immediately and no more checking will be performed.
#'
#'   The immediate return behavior can be helpful when you have to perform
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
#'     if (nrow(.result) != 5 && ncol(.result) < 2) {
#'       fail("Your table should have 5 rows and more than 1 column.")
#'     }
#'
#'     # ...and now we know it has 5 rows and at least 2 columns
#'     if (.result[[2]][[5]] != 5) {
#'       fail("The value of the 5th row of the 2nd column should be 5.")
#'     }
#'
#'     # all of the above checks have passed now.
#'     pass()
#'   })
#'   ```
#'   ````
#'
#'   Notice that it's important to choose a final fallback grade as the last
#'   value in your [grade_this()] checking code. This last value is the default
#'   grade that will be given if the submission passes all other checks. If
#'   you're using the standard [gradethis_setup()] and you call `pass()` or
#'   `fail()` without arguments, `pass()` will return a random praising phrase
#'   and `fail()` will return code feedback (if possible) with an encouraging
#'   phrase.
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
#'     # Fail with a hint if student code differs from the solution
#'     # (Skipped automatically if there isn't a -solution chunk)
#'     fail_if_code_feedback()
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
#' this_grader(mock_this_exercise(.user_code = "20 + 13", .solution_code = "20 + 22"))
#'
#' @param message A character string of the message to be displayed. In all
#'   grading helper functions other than [graded()], `message` is a template
#'   string that will be processed with [glue::glue()].
#' @param correct A logical value of whether or not the checked code is correct.
#' @param ... Additional arguments passed to `graded()` or otherwise ignored.
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
#' @return `pass()` signals a _correct_ submission, `fail()` signals an
#'   _incorrect_ submission, and `graded()` returns a correct or incorrect
#'   submission according to the value of `correct`.
#'
#' @seealso Grading helper functions
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
#'
#' @param env environment to evaluate the glue `message`. Most users of
#'   \pkg{gradethis} will not need to use this argument.
#' @param praise Include a random praising phrase with [random_praise()]? The
#'   default value of `praise` can be set using [gradethis_setup()] or the
#'   `gradethis.pass.praise` option.
#'
#' @export
pass <- function(
  message = getOption("gradethis.pass", "Correct!"),
  ...,
  env = parent.frame(),
  praise = getOption("gradethis.pass.praise", FALSE)
) {
  maybe_extras(
    graded(message = glue_message_with_env(env, message), correct = TRUE, ...),
    praise = praise
  )
}

#' @describeIn graded Signal a _failing_ grade.
#'
#' @param hint Include a code feedback hint with the failing message? This
#'   argument only applies to `fail()` and `fail_if_equal()` and the message is
#'   added using the default options of [give_code_feedback()] and
#'   [maybe_code_feedback()]. The default value of `hint` can be set using
#'   [gradethis_setup()] or the `gradethis.fail.hint` option.
#' @param encourage Incude a random encouraging phrase with
#'   [random_encouragement()]? The default value of `encourage` can be set
#'   using [gradethis_setup()] or the `gradethis.fail.encourage` option.
#'
#' @export
fail <- function(
  message = getOption("gradethis.fail", "Incorrect"),
  ...,
  env = parent.frame(),
  hint = getOption("gradethis.fail.hint", FALSE),
  encourage = getOption("gradethis.fail.encourage", FALSE)
) {
  maybe_extras(
    graded(message = glue_message_with_env(env, message), correct = FALSE, ...),
    env = env,
    hint = hint,
    encourage = encourage
  )
}

#' Signal a passing or failing grade if two values are equal
#' 
#' `pass_if_equal()` and `fail_if_equal()` are two [graded()] helper functions
#' that signal a passing or a failing grade if two values are equal. They are
#' designed to easily compare the returned value of the student's submitted
#' code with the value returned by the solution or another known value.
#' 
#' @inheritParams graded
#' @param x First item in the comparison. By default, when used inside
#'   [grade_this()], `x` is automatically assigned the value of `.result` — in
#'   other words the result of running the student's submitted code. `x` is not
#'   the first argument since you will often want to compare the final value of
#'   the student's submission against a specific value, `y`.
#' @param y The expected value against which `x` is compared using
#'   `waldo::compare(x, y)`. In `pass_if_equal()`, if no value is provided, the
#'   exercise `.solution`, or the result of evaluating the code in the
#'   exercise's `*-solution` chunk, will be used for the comparison.
#' @param ... Additional arguments passed to [graded()]
#' 
#' @return Returns a passing or failing grade if `x` and `y` are equal.
#' 
#' @seealso Grading helper functions
#' @describeIn pass_if_equal Signal a _passing_ grade only if `x` and `y` are
#'   equal.
#' @export
pass_if_equal <- function(
  y = missing_arg(),
  message = getOption("gradethis.pass", "Correct!"),
  x = missing_arg(),
  ...,
  env = parent.frame(),
  praise = getOption("gradethis.pass.praise", FALSE)
) {
  if (is_missing(x)) {
    x <- get_from_env(".result", env)
    if (is_missing(x)) {
      return(missing_object_in_env(".result", env, "pass_if_equal"))
    }
  }
  if (is_missing(y)) {
    y <- get_from_env(".solution", env)
    if (is_missing(y)) {
      return(missing_object_in_env(".solution", env, "pass_if_equal"))
    }
  }
  maybe_extras(
    grade_if_equal(x = x, y = y, message = message, correct = TRUE, env = env, ...),
    praise = praise
  )
}

#' @describeIn pass_if_equal Signal a _failing_ grade only if `x` and `y` are
#'   equal.
#' @export
fail_if_equal <- function(
  y,
  message = getOption("gradethis.fail", "Incorrect"),
  x = missing_arg(),
  ...,
  env = parent.frame(),
  hint = getOption("gradethis.fail.hint", FALSE),
  encourage = getOption("gradethis.fail.encourage", FALSE)
) {
  if (is_missing(x)) {
    x <- get_from_env(".result", env)
    if (is_missing(x)) {
      return(missing_object_in_env(".result", env, "fail_if_equal"))
    }
  }
  maybe_extras(
    grade_if_equal(x = x, y = y, message = message, correct = FALSE, env = env, ...),
    env = env,
    hint = hint,
    encourage = encourage
  )
}

grade_if_equal <- function(x, y, message, correct, env, ...) {
  compare_msg <- tryCatch(
    waldo::compare(x, y),
    error = function(e) {
      # https://github.com/brodieG/diffobj/issues/152#issuecomment-788083359
      # waldo::compare() calls diffobj::ses() — these functions try hard to create
      # a useable diff to describe the differences. These filters below cover
      # cases where the diff description throws an error, but we know they only
      # arise when a difference has occurred. Since we aren't (currently)
      # interested in reporting the differences between `x` and `y`, we mark
      # these as known to be different
      if (grepl("Exceeded buffer for finding fake snake", e$message, fixed = TRUE)) {
        "different"
      } else if (grepl("reached theoretically unreachable branch 2", e$message, fixed = TRUE)) {
        "different"
      } else {
        warning("Error in waldo::compare(): ", e$message, call. = FALSE)
        return(NULL)
      }
    }
  )

  if (is.null(compare_msg)) {
    return(graded(logical(), feedback_grading_problem()$message, type = "warning"))
  } else if (length(compare_msg) > 0) {
    # not equal! quit early
    return()
  }

  # equal!
  graded(message = glue_message_with_env(env, message), correct = correct, ...)
}

#' Signal a passing or failing grade if a condition is TRUE
#' 
#' @description
#' `pass_if()` and `fail_if()` both create passing or failing grades if a given
#' condition is `TRUE`. 
#' 
#' These functions are also used in legacy \pkg{gradethis} code, in particular
#' in the superseded function [grade_result()]. While previous versions of
#' \pkg{gradethis} allowed the condition to be determined by a function or
#' formula, when used in [grade_this()] the condition must be a logical `TRUE`
#' or `FALSE`.
#' 
#' @examples 
#' # TODO: examples
#' 
#' @param cond For `pass_if()` and `fail_if()`: A logical value or an expression
#'   that will evaluate to a `TRUE` or `FALSE` value. If the value is `TRUE`, or
#'   would be considered `TRUE` in an `if (cond)` statement, then a passing or
#'   failing grade is returned to the user.
#' @param x Deprecated. Replaced with `cond`.
#' @param ... Ignored
#' @inheritParams graded
#' 
#' @return `pass_if()` and `fail_if()` signal a correct or incorrect grade if
#'   the provided condition is `TRUE`.
#'   
#' @seealso Grading helper functions
#' @describeIn pass_if Pass if `cond` is `TRUE`.
#' @export
pass_if <- function(
  cond,
  message = NULL,
  ...,
  env = parent.frame(),
  praise = getOption("gradethis.pass.praise", FALSE),
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
      maybe_extras(pass(message, env = env), praise = praise)
    }
  } else {
    condition(cond, message, correct = TRUE)
  }
}

#' @describeIn pass_if Fail if `cond` is `TRUE`.
#' @export
fail_if <- function(
  cond,
  message = NULL,
  ...,
  env = parent.frame(),
  hint = getOption("gradethis.fail.hint", FALSE),
  encourage = getOption("gradethis.fail.encourage", FALSE),
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
      maybe_extras(
        fail(message, env = env),
        env = env,
        hint = hint,
        encourage = encourage
      )
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

#' Signal a failing grade if mistakes are detected in the submitted code
#' 
#' `fail_if_code_feedback()` uses [code_feedback()] to detect if there are
#' differences between the user's submitted code and the solution code (if
#' available). If the exercise does not have an associated solution, or if there
#' are no detected differences between the user's and the solution code, no
#' grade is returned.
#' 
#' @examples
#' # TODO: examples
#' 
#' @inheritParams code_feedback
#' @inheritParams graded
#' @inheritDotParams graded
#' 
#' @return Signals an incorrect grade with feedback if there are differences
#'   between the submitted user code and the solution code. If solution code is
#'   not available, no grade is returned.
#'
#' @seealso Grading helper functions
#' @export
fail_if_code_feedback <- function(
  user_code = missing_arg(),
  solution_code = missing_arg(),
  message = NULL,
  ...,
  env = parent.frame(),
  hint = TRUE,
  encourage = getOption("gradethis.fail.encourage", FALSE),
  allow_partial_matching = getOption("gradethis.allow_partial_matching", TRUE)
) {
  if (is_missing(user_code)) {
    user_code <- get_from_env(".user_code", env)
    if (is_missing(user_code)) {
      return(missing_object_in_env(".user_code", env, "fail_if_code_feedback"))
    }
    if (is.null(user_code) || length(user_code) == 0 || !nzchar(user_code)) {
      graded(logical(), "I didn't receive your code. Did you write any?", type = "info")
    }
  }
  if (is_missing(solution_code)) {
    solution_code <- get_from_env(".solution_code", env)
    if (is_missing(solution_code)) {
      # warn about missing solution code, but don't emit grade
      capture_graded(missing_object_in_env(".solution_code", env, "fail_if_code_feedback"))
    }
    if (
      is_missing(solution_code) || 
        is.null(solution_code) || 
        length(solution_code) == 0 || 
        !nzchar(solution_code)
    ) {
      # user_code can't be missing, but don't fail if solution code is missing
      return()
    }
  }
  env_feedback <- get0(".envir_prep", env, ifnotfound = env)
  
  feedback <- code_feedback(
    user_code = user_code,
    solution_code = solution_code,
    env = env_feedback,
    allow_partial_matching = allow_partial_matching
  )

  if (is.null(feedback)) {
    return()
  }

  if (!isTRUE(hint)) {
    feedback <- ""
  }
  
  message <- glue_with_env(env, message %||% "")
  if (nzchar(message) && nzchar(feedback)) {
    message <- paste0(message, " ")
  }
  
  maybe_extras(
    graded(FALSE, glue::glue("{message}{feedback}"), ...),
    env = env,
    # Don't add hint, we're already providing it directly
    hint = FALSE,
    encourage = encourage
  )
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
  get0(x, envir = env, ifnotfound = missing_arg())
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

maybe_extras <- function(
  expr,
  env = NULL,
  hint = FALSE,
  praise = FALSE,
  encourage = FALSE
) {
  # praise and encourage arguments win over inline praise/encouragement
  tmp_opts <- list()
  if (isTRUE(praise)) {
    tmp_opts[["gradethis.__praise"]] <- FALSE
  }
  if (isTRUE(encourage)) {
    tmp_opts[["gradethis.__encouragement"]] <- FALSE
  }

  grade <- with_options(
    tmp_opts,
    capture_graded(expr)
  )

  if (isTRUE(praise)) {
    grade <- capture_graded(give_praise(grade, location = "before"))
  }
  if (isTRUE(hint)) {
    grade <- capture_graded(give_code_feedback_(grade, env = env))
  }
  if (isTRUE(encourage)) {
    grade <- capture_graded(give_encouragement(grade, location = "after"))
  }

  signal_grade(grade)
}
