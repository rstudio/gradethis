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

#' Signal a passing or failing grade if two values are equal
#'
#' @description
#' `pass_if_equal()` and `fail_if_equal()` are two [graded()] helper functions
#' that signal a passing or a failing grade if two values are equal. They are
#' designed to easily compare the returned value of the student's submitted
#' code with the value returned by the solution or another known value:
#'
#' - Both functions find and use `.result` as the default for `x`, the first
#'   item in the comparison. `.result` is the last value returned from the
#'   user's submitted code.
#' - `pass_if_equal()` additionally finds and uses `.solution` as the default
#'   expected value `y`.
#'
#' See [graded()] for more information on \pkg{gradethis} grade-signaling
#' functions.
#'
#' @section Comparing with Multiple Solutions:
#' If your exercise includes multiple solutions that are variations of the same
#' task — meaning that all solutions achieve the same result — you can call
#' `pass_if_equal()` without changing any defaults to compare the result of the
#' student's submission to the common solution result. After checking if any
#' solution matches, you can perform additional checks or you can call [fail()]
#' with the [default message][gradethis_setup()] or with `hint = TRUE`. [fail()]
#' will automatically provide code feedback for the most likely solution.
#'
#' By default, `pass_if_equal()` will compare [.result] with [.solution], or the
#' final value returned by the entire `-solution` chunk (in other words, the
#' last solution). This default behavior covers both exercises with a single
#' solution and exercises with multiple solutions that all return the same
#' value.
#'
#' When your exercise has multiple solutions with **different results**,
#' `pass_if_equal()` can compare the student's [.result] to each of the
#' solutions in [.solution_all], returning a passing grade when the result
#' matches any of the values returned by the set of solutions. You can opt into
#' this behavior by calling
#'
#' ```r
#' pass_if_equal(.solution_all)
#' ```
#'
#' Note that this causes `pass_if_equal()` to evaluate each of the solutions in
#' the set, and may increase the computation time.
#'
#' Here's a small example. Suppose an exercise asks students to filter `mtcars`
#' to include only cars with the same number of cylinders. Students are free to
#' pick cars with 4, 6, or 8 cylinders, and so your `-solution` chunk would
#' include this code (ignoring the `ex_solution` variable, the chunk would
#' contain the code in the string below):
#'
#' ```{r}
#' ex_solution <- "
#' # four cylinders ----
#' mtcars[mtcars$cyl == 4, ]
#'
#' # six cylinders ----
#' mtcars[mtcars$cyl == 6, ]
#'
#' # eight cylinders ----
#' mtcars[mtcars$cyl == 8, ]
#' "
#' ```
#'
#' In the `-check` chunk, you'd call [grade_this()] and ask `pass_if_equal()` to
#' compare the student's [.result] to [.solution_all] (all the solutions).
#'
#' ```{r}
#' ex_check <- grade_this({
#'   pass_if_equal(
#'     y = .solution_all,
#'     message = "The cars in your result all have {.solution_label}!"
#'    )
#'
#'   fail()
#' })
#' ```
#'
#' What happens when a student submits one of these solutions? This function
#' below mocks the process of a student submitting an attempt.
#'
#' ```{r}
#' student_submits <- function(code) {
#'   withr::local_seed(42)
#'   submission <- mock_this_exercise(!!code, !!ex_solution)
#'   ex_check(submission)
#' }
#' ```
#'
#' If they submit code that returns one of the three possible solutions, they
#' receive positive feedback.
#'
#' ```{r}
#' student_submits("mtcars[mtcars$cyl == 4, ]")
#' student_submits("mtcars[mtcars$cyl == 6, ]")
#' ```
#'
#' Notice that the solution label appears in the feedback message. When
#' `pass_if_equal()` picks a solution as correct, three variables are made
#' available for use in the glue string provided to `message`:
#'
#' * `.solution_label`: The heading label of the matching solution
#' * `.solution_code`: The code of the matching solution
#' * `.solution`: The value of the evaluated matching solution code
#'
#' If the student submits incorrect code, `pass_if_equal()` defers to later
#' grading code.
#'
#' ```{r}
#' student_submits("mtcars[mtcars$cyl < 8, ]")
#' ```
#'
#' Here, because [fail()] provides [code_feedback()] by default, and because
#' [code_feedback()] is also aware of the multiple solutions for this exercise,
#' the code feedback picks the _eight cylinders_ solution and gives advice
#' based on that particular solution.
#'
#' @examples
#' # Suppose our prompt is to find the cars in `mtcars` with 6 cylinders...
#'
#' grader <-
#'   # ```{r example-check}
#'   grade_this({
#'     # Automatically pass if .result equal to .solution
#'     pass_if_equal()
#'
#'     fail_if_equal(mtcars[mtcars$cyl == 4, ], message = "Not four cylinders")
#'     fail_if_equal(mtcars[mtcars$cyl == 8, ], message = "Not eight cylinders")
#'
#'     # Default to failing grade with feedback
#'     fail()
#'   })
#' # ```
#'
#' .solution <-
#'   # ```{r example-solution}
#'   mtcars[mtcars$cyl == 6, ]
#' # ```
#'
#' # Correct!
#' grader(mock_this_exercise(mtcars[mtcars$cyl == 6, ], !!.solution))
#'
#' # These fail with specific messages
#' grader(mock_this_exercise(mtcars[mtcars$cyl == 4, ], !!.solution))
#' grader(mock_this_exercise(mtcars[mtcars$cyl == 8, ], !!.solution))
#'
#' # This fails with default feedback message
#' grader(mock_this_exercise(mtcars[mtcars$mpg == 8, ], !!.solution))
#' @inheritParams graded
#' @param x First item in the comparison. By default, when used inside
#'   [grade_this()], `x` is automatically assigned the value of `.result` — in
#'   other words the result of running the student's submitted code. `x` is not
#'   the first argument since you will often want to compare the final value of
#'   the student's submission against a specific value, `y`.
#' @param y The expected value against which `x` is compared using
#'   `waldo::compare(x, y)`.
#'
#'   In `pass_if_equal()`, if no value is provided, the exercise `.solution`
#'   (i.e. the result of evaluating the code in the exercise's `*-solution`
#'   chunk) will be used for the comparison.
#'
#'   If the exercise uses multiple solutions with _different results_, set
#'   `y = .solution_all`. In this case, `pass_if_equal()` will test each of the
#'   solutions and provide a passing grade if `x` matches _any_ values contained
#'   in `y`. Note that if the exercise has multiple solutions but they all
#'   return the same result, it will be faster to use the default value of
#'   `y = .solution`.
#' @inheritParams waldo::compare
#' @param ... Additional arguments passed to [graded()]
#'
#' @return Returns a passing or failing grade if `x` and `y` are equal.
#'
#' @template graded-family
#' @describeIn pass_if_equal Signal a _passing_ grade only if `x` and `y` are
#'   equal.
#' @export
pass_if_equal <- function(
  y = .solution,
  message = getOption("gradethis.pass", "Correct!"),
  x = .result,
  ...,
  env = parent.frame(),
  tolerance = sqrt(.Machine$double.eps),
  praise = getOption("gradethis.pass.praise", FALSE)
) {

  x <- resolve_placeholder(x, env)
  # in general: try .solution_all, fall back to .solution
  y <- resolve_placeholder(y, env, default = resolve_placeholder(.solution, env))

  grade_if_equal_p <- purrr::partial(
    grade_if_equal,
    message = message,
    correct = TRUE,
    tolerance = tolerance,
    ...
  )

  if (inherits(y, "gradethis_solutions_env")) {
    # Code for multiple solutions is a named list with user labels as names. The
    # labels may not be unique, so for .solution_code_all items should be
    # accessed by position. OTOH for `.solution_all` we use an environment,
    # rather than a list, so positional indexing isn't possible. In the
    # `.solution_all` environment we store the original solution labels with
    # their associated labels as a named character vector. The names are the
    # unique binding names in `.solution_all` and the values are the original,
    # potentially non-unique labels provided by the user.
    solution_labels <- get_from_env(".solution_labels", y)

    for (i in seq_along(solution_labels)) {
      # get solution code **by index** (see above)
      solution_code <- get_from_env(".solution_code_all", env)[[i]]

      solution_name <- names(solution_labels)[i]
      solution_label <- unname(solution_labels[solution_name])

      # get solution **by name** (see above)
      solution <- y[[solution_name]]

      # these extras become available for the glue string (or override defaults)
      solution_env_extras <- list(
        .solution = solution,
        .solution_code = if (!rlang::is_missing(solution_code)) solution_code,
        .solution_label = solution_label
      )

      env_soln <- rlang::new_environment(
        purrr::compact(solution_env_extras),
        parent = env
      )

      maybe_extras(
        grade_if_equal_p(
          x = x,
          y = solution,
          env = env_soln,
          solution_label = solution_label,
          solution_index = i
        ),
        praise = praise
      )
    }
  } else {
    maybe_extras(
      grade_if_equal_p(x = x, y = y, env = env),
      praise = praise
    )
  }
}

#' @describeIn pass_if_equal Signal a _failing_ grade only if `x` and `y` are
#'   equal.
#' @export
fail_if_equal <- function(
  y,
  message = getOption("gradethis.fail", "Incorrect"),
  x = .result,
  ...,
  env = parent.frame(),
  tolerance = sqrt(.Machine$double.eps),
  hint = getOption("gradethis.fail.hint", FALSE),
  encourage = getOption("gradethis.fail.encourage", FALSE)
) {
  x <- resolve_placeholder(x, env)

  maybe_extras(
    grade_if_equal(
      x = x,
      y = y,
      message = message,
      correct = FALSE,
      env = env,
      tolerance = tolerance,
      ...
    ),
    env = env,
    hint = hint,
    encourage = encourage
  )
}

grade_if_equal <- function(
  x, y, message, correct, env, tolerance = sqrt(.Machine$double.eps), ...
) {
  local_options_waldo_compare()

  compare_msg <- tryCatch(
    waldo::compare(x, y, tolerance = tolerance),
    error = function(e) {
      # https://github.com/brodieG/diffobj/issues/152#issuecomment-788083359
      # waldo::compare() calls diffobj::ses() — these functions try hard to create
      # a usable diff to describe the differences.
      # We're engaging in some off-label usage of these functions,
      # so they will sometimes error when we give them unusual inputs.
      # If these functions throw an error, we fall back to `identical()`.
      # Since we aren't (currently)
      # interested in reporting the differences between `x` and `y`, we mark
      # these cases as simply "different" if they aren't identical.
      if (!identical(x, y)) "different"
    }
  )

  if (is_graded(compare_msg)) {
    # an internal grading problem occurred with waldo::compare()
    return(compare_msg)
  }

  if (length(compare_msg) > 0) {
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
    assert_gradethis_condition_type_is_value(cond, "pass_if")
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
    assert_gradethis_condition_type_is_value(cond, "fail_if")
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

assert_gradethis_condition_type_is_value <- function(x, from = NULL) {
  type <- condition_type(x)
  if (!identical(type, "value")) {
    from <- if (!is.null(from)) paste0(from, "() ") else ""
    msg_internal <- paste0(from, "does not accept functions or formulas when used inside grade_this().")
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
