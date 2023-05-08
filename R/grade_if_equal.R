grade_if_equal <- function(
  x,
  y,
  message,
  correct,
  env,
  tolerance = sqrt(.Machine$double.eps),
  ...
) {
  comparison <- gradethis_equal(x, y, tolerance)

  if (is_graded(comparison)) {
    # an internal grading problem occurred
    return(comparison)
  }

  if (is_false(comparison)) {
    # not equal! quit early
    return()
  }

  # equal!
  graded(message = glue_message_with_env(env, message), correct = correct, ...)
}

grade_if_not_equal <- function(
  x,
  y,
  message,
  correct,
  env,
  tolerance = sqrt(.Machine$double.eps),
  ...
) {
  comparison <- gradethis_equal(x, y, tolerance)

  if (is_graded(comparison)) {
    # an internal grading problem occurred
    return(comparison)
  }

  if (is_true(comparison)) {
    return()
  }

  graded(message = glue_message_with_env(env, message), correct = correct, ...)
}

#' Signal a passing or failing grade if two values are equal
#'
#' @description
#' `pass_if_equal()`, `fail_if_equal()`, and `fail_if_not_equal()`
#' are three [graded()] helper functions
#' that signal a passing or a failing grade
#' based on the whether two values are equal.
#' They are designed to easily compare
#' the returned value of the student's submitted code
#' with the value returned by the solution or another known value:
#'
#' - Each function finds and uses `.result` as the default for `x`,
#'   the first item in the comparison.
#'   `.result` is the last value returned from the user's submitted code.
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
#'   `gradethis_equal(x, y)`.
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
#' @inheritParams gradethis_equal
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

#' @describeIn pass_if_equal Signal a _failing_ grade if `x` and `y` are _not_
#'   equal.
#' @export
fail_if_not_equal <- function(
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
    grade_if_not_equal(
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
