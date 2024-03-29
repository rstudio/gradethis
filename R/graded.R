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
#'   4. All helper functions include an `env` parameter, that you can generally
#'      ignore. It's used internally to help `pass()` and `fail()` _et al._ find
#'      the default argument values and to build the `message` using
#'      [glue::glue()].
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
#' # save the function returned by `grade_this()` as `grader()`. Calling
#' # `grader()` on a mock exercise submission is equivalent to running the
#' # check code when the student clicks "Submit Answer" in a learnr tutorial.
#'
#' grader <-
#'   # ```{r example-check}
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
#' grader(mock_this_exercise(.user_code = 42))
#'
#' # These were close...
#' grader(mock_this_exercise(.user_code = 41))
#' grader(mock_this_exercise(.user_code = 43))
#'
#' # Automatically use .solution if you have a *-solution chunk...
#' grader(mock_this_exercise(.user_code = 42, .solution_code = 42))
#'
#' # Floating point arithmetic is tricky...
#' grader(mock_this_exercise(.user_code = 42.000001, .solution_code = 42))
#' grader(mock_this_exercise(.user_code = 64.123456, .solution_code = 42))
#'
#' # Complicated checking situations...
#' grader(mock_this_exercise(.user_code = 101, .solution_code = 42))
#' grader(mock_this_exercise(.user_code = 0.42, .solution_code = 42))
#'
#' # Finally fall back to the final answer...
#' grader(mock_this_exercise(.user_code = "20 + 13", .solution_code = "20 + 22"))
#' @param message A character string of the message to be displayed. In all
#'   grading helper functions other than [graded()], `message` is a template
#'   string that will be processed with [glue::glue()].
#' @param correct A logical value of whether or not the checked code is correct.
#' @param ... Additional arguments passed to `graded()` or additional data to be
#'   included in the feedback object.
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
#' @template graded-family
#' @describeIn graded Prepare and signal a graded result.
#' @export
graded <- function(correct, message = NULL, ..., type = NULL, location = NULL) {
  if (length(list(...))) {
    # ... must be unique and named
    checkmate::assert_names(names(list(...)), "unique", .var.name = "...")
  }

  # allow logical(0) to signal a neutral grade
  checkmate::assert_logical(correct, any.missing = FALSE, max.len = 1, null.ok = FALSE)

  obj <- structure(
    list(
      message = message %||% "",
      correct = correct,
      type = type,
      location = location,
      ...
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
#' @param encourage Include a random encouraging phrase with
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
  user_provided_hint <- !missing(hint)
  user_provided_message <- !missing(message)

  this_fail_grade <- function() {
    if (user_provided_hint && identical(hint, FALSE) && !user_provided_message) {
      # allow hint = FALSE, provided by the user, to override the default message
      with_maybe_code_feedback(
        FALSE,
        graded(message = glue_message_with_env(env, message), correct = FALSE, ...)
      )
    } else {
      graded(message = glue_message_with_env(env, message), correct = FALSE, ...)
    }
  }

  maybe_extras(
    this_fail_grade(),
    env = env,
    hint = hint,
    encourage = encourage
  )
}

#' Signal a passing or failing grade if a condition is TRUE
#'
#' @description
#' `pass_if()` and `fail_if()` both create passing or failing grades if a given
#' condition is `TRUE`. See [graded()] for more information on \pkg{gradethis}
#' grade-signaling functions.
#'
#' These functions are also used in legacy \pkg{gradethis} code, in particular
#' in the superseded function [grade_result()]. While previous versions of
#' \pkg{gradethis} allowed the condition to be determined by a function or
#' formula, when used in [grade_this()] the condition must be a logical `TRUE`
#' or `FALSE`.
#'
#' @examples
#' # Suppose the prompt is to find landmasses in `islands` with land area of
#' # less than 20,000 square miles. (`islands` reports land mass in units of
#' # 10,000 sq. miles.)
#'
#' grader <-
#'   # ```{r example-check}
#'   grade_this({
#'     fail_if(any(is.na(.result)), "You shouldn't have missing values.")
#'
#'     diff_len <- length(.result) - length(.solution)
#'     fail_if(diff_len < 0, "You missed {abs(diff_len)} island(s).")
#'     fail_if(diff_len > 0, "You included {diff_len} too many islands.")
#'
#'     pass_if(all(.result < 20), "Great work!")
#'
#'     # Fall back grade
#'     fail()
#'   })
#' # ```
#'
#' .solution <-
#'   # ```{r example-solution}
#'   islands[islands < 20]
#' # ```
#'
#' # Peek at the right answer
#' .solution
#'
#' # Has missing values somehow
#' grader(mock_this_exercise(islands["foo"], !!.solution))
#'
#' # Has too many islands
#' grader(mock_this_exercise(islands[islands < 29], !!.solution))
#'
#' # Has too few islands
#' grader(mock_this_exercise(islands[islands < 16], !!.solution))
#'
#' # Just right!
#' grader(mock_this_exercise(islands[islands < 20], !!.solution))
#' @param cond A logical value or an expression that will evaluate to a `TRUE`
#'   or `FALSE` value. If the value is `TRUE`, or would be considered `TRUE` in
#'   an `if (cond)` statement, then a passing or failing grade is returned to
#'   the user.
#' @param x Deprecated. Replaced with `cond`.
#' @param ... Passed to [graded()] in [grade_this()].
#' @inheritParams graded
#'
#' @return `pass_if()` and `fail_if()` signal a correct or incorrect grade if
#'   the provided condition is `TRUE`.
#'
#' @template graded-family
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
    assert_gradethis_condition_is_true_or_false(cond, "pass_if")
    if (cond) {
      message <- message %||% getOption("gradethis.pass", "Correct!")
      pass(message, env = env, ..., praise = praise)
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
    assert_gradethis_condition_is_true_or_false(cond, "fail_if")
    if (cond) {
      message <- message %||% getOption("gradethis.fail", "Incorrect.")
      fail(message, env = env, ..., hint = hint, encourage = encourage)
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
#' @description
#' `fail_if_code_feedback()` uses [code_feedback()] to detect if there are
#' differences between the user's submitted code and the solution code (if
#' available). If the exercise does not have an associated solution, or if there
#' are no detected differences between the user's and the solution code, no
#' grade is returned.
#'
#' See [graded()] for more information on \pkg{gradethis} grade-signaling
#' functions.
#'
#' @examples
#' # Suppose the exercise prompt is to generate 5 random numbers, sampled from
#' # a uniform distribution between 0 and 1. In this exercise, you know that
#' # you shouldn't have values outside of the range of 0 or 1, but you'll
#' # otherwise need to check the submitted code to know that the student has
#' # chosen the correct sampling function.
#'
#' grader <-
#'   # ```{r example-check}
#'   grade_this({
#'     fail_if(length(.result) != 5, "I expected 5 numbers.")
#'     fail_if(
#'       any(.result < 0 | .result > 1),
#'       "I expected all numbers to be between 0 and 1."
#'     )
#'
#'     # Specific checks passed, but now we want to check the code.
#'     fail_if_code_feedback()
#'
#'     # All good!
#'     pass()
#'   })
#' # ```
#'
#' .solution_code <- "
#' # ```{r example-check}
#'   runif(5)
#' # ```
#' "
#'
#' # Not 5 numbers...
#' grader(mock_this_exercise(runif(1), !!.solution_code))
#'
#' # Not within [0, 1]...
#' grader(mock_this_exercise(rnorm(5), !!.solution_code))
#'
#' # Passes specific checks, but hard to tell so check the code...
#' grader(mock_this_exercise(runif(5, 0.25, 0.75), !!.solution_code))
#' grader(mock_this_exercise(rbinom(5, 1, 0.5), !!.solution_code))
#'
#' # Perfect!
#' grader(mock_this_exercise(runif(n = 5), !!.solution_code))
#'
#' @inheritParams code_feedback
#' @inheritParams graded
#' @inheritDotParams graded
#'
#' @return Signals an incorrect grade with feedback if there are differences
#'   between the submitted user code and the solution code. If solution code is
#'   not available, no grade is returned.
#'
#' @template graded-family
#' @export
fail_if_code_feedback <- function(
  message = NULL,
  user_code = .user_code,
  solution_code = .solution_code_all,
  ...,
  env = parent.frame(),
  hint = TRUE,
  encourage = getOption("gradethis.fail.encourage", FALSE),
  allow_partial_matching = getOption("gradethis.allow_partial_matching", TRUE)
) {
  user_code <- resolve_placeholder(user_code, env)

  if (is_empty_code(user_code)) {
    return(grade_code_is_empty())
  }

  solution_code <- resolve_placeholder(solution_code, env, throw_grade = FALSE)

  is_missing_solution_code <-
    is_placeholder(solution_code) ||
      is_missing(solution_code) ||
      is.null(solution_code) ||
      length(solution_code) == 0 ||
      !any(nzchar(solution_code))

  if (is_missing_solution_code) {
    # user_code can't be missing, but don't fail if solution code is missing
    return()
  }

  user_env <- get0(".envir_result", env, ifnotfound = env)
  solution_env <- get0(".envir_result", env, ifnotfound = env)

  feedback <- code_feedback(
    user_code = user_code,
    solution_code = solution_code,
    user_env = user_env,
    solution_env = solution_env,
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

#' Fail if grading code produces an error
#'
#' When grading code involves unit-style testing, you may want to use
#' \pkg{testthat} expectation function to test the user's submitted code. In
#' these cases, to differentiate between expected errors and internal errors
#' indicative of issues with the grading code, \pkg{gradethis} requires that
#' authors wrap assertion-style tests in `fail_if_error()`. This function
#' catches any errors and converts them into [fail()] grades. It also makes the
#' error and its message available for use in the `message` glue string as
#' `.error` and `.error_message` respectively.
#'
#' @examples
#' # The user is asked to add 2 + 2, but they take a shortcut
#' ex <- mock_this_exercise("'4'")
#'
#' # Normally, grading code with an author error returns an internal problem grade
#' grade_author_mistake <- grade_this({
#'   if (identical(4)) {
#'     pass("Great work!")
#'   }
#'   fail()
#' })(ex)
#'
#' # This returns a "problem occurred" grade
#' grade_author_mistake
#' # ...that also includes information about the error (not shown to users)
#' grade_author_mistake$error
#'
#' # But sometimes we'll want to use unit-testing helper functions where we know
#' # that an error is indicative of a problem in the users' code
#' grade_this({
#'   fail_if_error({
#'     testthat::expect_length(.result, 1)
#'     testthat::expect_true(is.numeric(.result))
#'     testthat::expect_equal(.result, 4)
#'   })
#'   pass("Good job!")
#' })(ex)
#'
#' # Note that you don't need to reveal the error message to the user
#' grade_this({
#'   fail_if_error(
#'     message = "Your result isn't a single numeric value.",
#'     {
#'       testthat::expect_length(.result, 1)
#'       testthat::expect_true(is.numeric(.result))
#'       testthat::expect_equal(.result, 4)
#'     }
#'   )
#'   pass("Good job!")
#' })(ex)
#' @param expr An expression to evaluate that whose errors are safe to be
#'   converted into failing grades with [fail()].
#' @param message A glue string containing the feedback message to be returned
#'   to the user. Additional `.error` and `.error_message` objects are made
#'   available for use in the message.
#' @inheritParams fail
#'
#' @return If an error occurs while evaluating `expr`, the error is returned as
#'   a [fail()] grade. Otherwise, no value is returned.
#'
#' @template graded-family
#' @export
fail_if_error <- function(
  expr,
  message = "{.error_message}",
  ...,
  env = parent.frame(),
  hint = TRUE,
  encourage = getOption("gradethis.fail.encourage", FALSE)
) {
  grade <-
    capture_errors(
      expr,
      on_error = gradethis_fail_error_handler(
        message = message,
        env = env,
        hint = hint,
        encourage = encourage,
        ...
      )
    )
  if (is_graded(grade)) {
    signal_grade(grade)
  }
}

assert_gradethis_condition_is_true_or_false <- function(cond, from = NULL) {
  from <- if (!is.null(from)) paste0("to `", from, "()` ") else ""

  assert_gradethis_condition_does_not_error(cond, from)
  assert_gradethis_condition_type_is_value(cond, from)
  assert_gradethis_condition_is_scalar(cond, from)
  assert_gradethis_condition_is_logical(cond, from)
  assert_gradethis_condition_is_not_na(cond, from)
}

assert_gradethis_condition_does_not_error <- function(cond, from) {
  error <- rlang::catch_cnd(cond, "error")

  if (rlang::is_error(error)) {
    msg_internal <- paste0(
      "The `cond` argument ", from, "produced an error:", "\n",
      "  Error in ", format(error$call), " : ", error$message
    )

    warning(msg_internal, immediate. = TRUE, call. = !is.null(from))
    grade_grading_problem(error = error)
  }
}

assert_gradethis_condition_type_is_value <- function(cond, from) {
  type <- condition_type(cond)

  if (!identical(type, "value")) {
    msg_internal <- paste0(
      "The `cond` argument ",
      from,
      "does not accept functions or formulas when used inside `grade_this()`."
    )

    warning(msg_internal, immediate. = TRUE, call. = !is.null(from))
    grade_grading_problem(error = list(message = msg_internal))
  }
}

assert_gradethis_condition_is_scalar <- function(cond, from) {
  cond_length <- length(cond)

  if (cond_length != 1) {
    msg_internal <- paste0(
      "The `cond` argument ", from, "must be length 1, ",
      "not ", cond_length, "."
    )

    warning(msg_internal, immediate. = TRUE, call. = !is.null(from))
    grade_grading_problem(error = list(message = msg_internal))
  }
}

assert_gradethis_condition_is_logical <- function(cond, from) {
  cond_class <- paste0("<", paste(class(cond), collapse = "/"), ">")

  if (!inherits(cond, "logical") && identical(as.logical(cond), NA)) {
    msg_internal <- paste0(
      "The `cond` argument ", from, "must be coercible to logical, ",
      "not an object of class ", cond_class, "."
    )

    warning(msg_internal, immediate. = TRUE, call. = !is.null(from))
    grade_grading_problem(error = list(message = msg_internal))
  }
}

assert_gradethis_condition_is_not_na <- function(cond, from) {
  if (identical(cond, NA)) {
    msg_internal <- paste0("The `cond` argument ", from, "must not be `NA`.")
    warning(msg_internal, immediate. = TRUE, call. = !is.null(from))
    grade_grading_problem(error = list(message = msg_internal))
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
