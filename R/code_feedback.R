
#' Provide automated code feedback
#'
#' Generate a message describing the first instance of a code mismatch. Three
#' functions are provided for working with code feedback: `code_feedback()` does
#' the comparison and returns a character description of the mismatch, or a
#' `NULL` if no differences are found. `maybe_code_feedback()` is designed to be
#' used inside [fail()] and related [graded()] messages, as in
#' `"{maybe_code_feedback()}"`. And `give_code_feedback()` gives you a way to
#' add code feedback to any [fail()] message in a [grade_this()] or
#' [grade_result()] checking function.
#'
#' @section Code differences:
#'
#' There are many different ways that code can be different, yet still the same.
#' Here is how we detect code differences:
#'
#' 1. If the single values are different. Ex: `log(2)` vs `log(3)`
#' 2. If the function call is different. Ex: `log(2)` vs `sqrt(2)`
#' 3. Validate the user code can be standardized via
#'    [rlang::call_standardise()]. The `env` parameter is important for this
#'    step as \pkg{gradethis} does not readily know about user defined
#'    functions. Ex: `read.csv("file.csv")` turns into
#'    `read.csv(file = "file.csv")`
#' 4. If multiple formals are matched. Ex: `read.csv(f = "file.csv")` has `f`
#'    match to `file` and `fill`.
#' 5. Verify that every named argument in the solution appears in the user
#'    code. Ex: If the solution is `read.csv("file.csv", header = TRUE)`,
#'    `header` must exist.
#' 6. Verify that the user did not supply extra named arguments to `...`.
#'    Ex: `mean(x = 1:10, na.rm = TRUE)` vs `mean(x = 1:10)`
#' 7. Verify that every named argument in the solution matches the value of the
#'    corresponding user argument. Ex: `read.csv("file.csv", header = TRUE)`
#'    vs `read.csv("file.csv", header = FALSE)`
#' 8. Verify that the remaining arguments of the user and solution code match
#'    in order and value. Ex: `mean(1:10, 0.1)` vs `mean(1:10, 0.2)`
#'
#' @examples
#' # code_feedback() ------------------------------------------------------
#'
#' # Values are same, so no differences found
#' code_feedback("log(2)", "log(2)")
#'
#' # Functions are different
#' code_feedback("log(2)", "sqrt(2)")
#'
#' # Standardize argument names (no differences)
#' code_feedback("read.csv('file.csv')", "read.csv(file = 'file.csv')")
#'
#' # Partial matching is not allowed
#' code_feedback("read.csv(f = 'file.csv')", "read.csv(file = 'file.csv')")
#'
#' # Feedback will spot differences in argument values...
#' code_feedback(
#'   "read.csv('file.csv', header = FALSE)",
#'   "read.csv('file.csv', header = TRUE)"
#' )
#'
#' # ...or when arguments are expected to appear in a call...
#' code_feedback("mean(1:10)", "mean(1:10, na.rm = TRUE)")
#'
#' # ...even when the expected argument matches the function's default value
#' code_feedback("read.csv('file.csv')", "read.csv('file.csv', header = TRUE)")
#'
#' # Unstandardized arguments will match by order and value
#' code_feedback("mean(1:10, 0.1)", "mean(1:10, 0.2)")
#'
#'
#' # give_code_feedback() -------------------------------------------------
#'
#' # We'll use this example of an incorrect exercise submission throughout
#' submission_wrong <- mock_this_exercise(
#'   .user_code = "log(4)",
#'   .solution_code = "sqrt(4)"
#' )
#'
#' # To add feedback to *any* incorrect grade,
#' # wrap the entire `grade_this()` call in `give_code_feedback()`:
#' grader <-
#'   # ```{r example-check}
#'   give_code_feedback(grade_this({
#'     pass_if_equal(.solution, "Good job!")
#'     if (.result < 2) {
#'       fail("Too low!")
#'     }
#'     fail()
#'   }))
#' # ```
#' grader(submission_wrong)
#'
#' # Or you can wrap the message of any fail() directly:
#' grader <-
#'   # ```{r example-check}
#'   grade_this({
#'     pass_if_equal(.solution, "Good job!")
#'     if (.result < 2) {
#'       fail(give_code_feedback("Too low!"))
#'     }
#'     fail()
#'   })
#' # ```
#' grader(submission_wrong)
#'
#' # Typically, grade_result() doesn't include code feedback
#' grader <-
#'   # ```{r example-check}
#'   grade_result(
#'     fail_if(~ round(.result, 0) != 2, "Not quite!")
#'   )
#' # ```
#' grader(submission_wrong)
#'
#' # But you can use give_code_feedback() to append code feedback
#' grader <-
#'   # ```{r example-check}
#'   give_code_feedback(grade_result(
#'     fail_if(~ round(.result, 0) != 2, "Not quite!")
#'   ))
#' # ```
#' grader(submission_wrong)
#'
#' # The default `grade_this_code()` `incorrect` message always adds code feedback,
#' # so be sure to remove \"{maybe_code_feedback()}\" from the incorrect message
#' grader <-
#'   # ```{r example-check}
#'   give_code_feedback(grade_this_code(incorrect = "{random_encouragement()}"))
#' # ```
#' grader(submission_wrong)
#' @param user_code,solution_code String containing user or solution code. For
#'   ease of use in [grade_this()], [.user_code] or [.solution_code] are by
#'   default retrieved from the calling environment.
#' @param solution_code_all A list containing the code of all solutions when
#'   multiple solutions are provided. For ease of use in [grade_this()],
#'   [.solution_code_all] is by default retrieved from the calling environment.
#' @param env Environment used to standardize formals of the user and solution
#'   code. Defaults to retrieving [.envir_prep] from the calling environment. If
#'   not found, the [parent.frame()] will be used.
#' @param ... Ignored in `code_feedback()` and `maybe_code_feedback()`. In
#'   `give_code_feedback()`, `...` are passed to `maybe_code_feedback()`.
#' @param allow_partial_matching A logical. If `FALSE`, the partial matching of
#'   argument names is not allowed and e.g. `runif(1, mi = 0)` will return a
#'   message indicating that the full formal name `min` should be used. The
#'   default is set via the `gradethis.allow_partial_matching` option, or by
#'   [gradethis_setup()].
#'
#' @return
#'
#'   - `code_feedback()` returns a character value describing the difference
#'     between the student's submitted code and the solution. If no
#'     discrepancies are found, `code_feedback()` returns `NULL`.
#'
#'   - `maybe_code_feedback()` always returns a string for safe use in glue
#'     strings. If no discrepancies are found, it returns an empty string.
#'
#'   - `give_code_feedback()` catches [fail()] grades and adds code feedback to
#'      the feedback message using `maybe_code_feedback()`.
#'
#' @describeIn code_feedback Determine code feedback by comparing the user's
#'   code to the solution.
#' @export
code_feedback <- function(
  user_code = .user_code,
  solution_code = .solution_code,
  solution_code_all = .solution_code_all,
  env = .envir_prep,
  ...,
  allow_partial_matching = getOption("gradethis.allow_partial_matching", TRUE)
) {
  ellipsis::check_dots_empty()

  if (is_placeholder(env, ".envir_prep")) {
    env <- get0(".envir_prep", parent.frame(), ifnotfound = .envir_prep)
    if (is_placeholder(env)) {
      env <- parent.frame()
    }
    assert_not_placeholder(env)
  }
  if (is_placeholder(user_code, ".user_code")) {
    user_code <- get0(".user_code", parent.frame())
    assert_not_placeholder(user_code)
  }
  if (is_placeholder(solution_code_all, ".solution_code_all")) {
    solution_code_all <- get0(".solution_code_all", parent.frame())

    # If .solution_code_all is not present, create it from .solution_code
    if (is_placeholder(solution_code_all, ".solution_code_all")) {
      if (is_placeholder(solution_code, ".solution_code")) {
        solution_code <- get0(".solution_code", parent.frame())
        assert_not_placeholder(solution_code)
      }

      solution_code_all <- solutions_prepare(solution_code)
    }
  }

  closest_solution <- which_closest_solution_code(user_code, solution_code_all)
  solution_code <- solution_code_all[[closest_solution]]

  user_expr <- to_expr(user_code, "user_code")
  solution_expr <- to_expr(solution_code, "solution_code")
  checkmate::assert_environment(env, null.ok = FALSE, .var.name = "env")

  if (identical(user_expr, solution_expr)) {
    # identical! return early
    return(NULL)
  }

  # returns `NULL` if no mistakes are found
  detect_mistakes(
    user = user_expr,
    solution = solution_expr,
    env = new.env(parent = env),
    allow_partial_matching = isTRUE(allow_partial_matching)
  )
}

to_expr <- function(x, name) {
  if (rlang::is_quosure(x)) {
    as.expression(rlang::get_expr(x))
  } else {
    checkmate::assert_character(x, null.ok = FALSE, min.chars = 1L, min.len = 1, .var.name = name)
    str2expression(x)
  }
}

should_display_code_feedback <- function() {
  isTRUE(getOption("gradethis.maybe_code_feedback", FALSE))
}

with_maybe_code_feedback <- function(val, expr) {
  with_options(
    list("gradethis.maybe_code_feedback" = val),
    expr
  )
}

which_closest_solution_code <- function(user_code, solution_code_all) {
  # If there's no solution code or only one solution,
  # we don't need to find the closest match
  if (length(solution_code_all) < 2) {
    return(length(solution_code_all))
  }

  # Convert from list to character vector
  solution_code_all <- unlist(solution_code_all)

  standardise_code_text <- function(code) {
    code %>%
      unpipe_all_str() %>%
      rlang::parse_exprs() %>%
      call_standardise_formals_recursive() %>%
      purrr::map_chr(rlang::expr_text) %>%
      paste(collapse = "\n")
  }

  user_code <- standardise_code_text(user_code)

  solution_code_all <- solution_code_all %>%
    purrr::map_chr(standardise_code_text)

  # Find the index of the solution code that the user code is closest to
  # which.min.last() uses the last index if there is a tie
  closest_solution <- which.min.last(utils::adist(user_code, solution_code_all))

  # If closest_solution is invalid,
  # fallback to the last element of solution_code_all
  if (
    length(closest_solution) != 1 ||
      !closest_solution %in% seq_along(solution_code_all)
  ) {
    closest_solution <- length(solution_code_all)
  }

  closest_solution
}

which.min.last <- function(x) {
  x <- rev(x)
  index <- which.min(x)
  rev(seq_along(x))[index]
}

#' @describeIn code_feedback Return `code_feedback()` result when possible.
#'   Useful when setting default [fail()] glue messages. For example, if there
#'   is no solution, no code feedback will be given.
#'
#' @param default Default value to return if no code feedback is found or code
#'   feedback can be provided.
#' @param before,after Strings to be added before or after the code feedback
#'   message to ensure the message is properly formatted in your feedback.
#' @param space_before,space_after Deprecated. Use `before` and `after`.
#'
#' @export
maybe_code_feedback <- function(
  user_code = get0(".user_code", parent.frame()),
  solution_code = get0(".solution_code", parent.frame()),
  solution_code_all = get0(".solution_code_all", parent.frame()),
  env = get0(".envir_prep", parent.frame(), ifnotfound = parent.frame()),
  ...,
  allow_partial_matching = getOption("gradethis.allow_partial_matching", TRUE),
  default = "",
  before = getOption("gradethis.maybe_code_feedback.before", " "),
  after = getOption("gradethis.maybe_code_feedback.after", NULL),
  space_before = deprecated(),
  space_after = deprecated()
) {
  ellipsis::check_dots_empty()

  # if feedback is not enabled, return
  if (!should_display_code_feedback()) {
    return(default)
  }

  if (is_present(space_before)) {
    deprecate_warn("0.2.3", "maybe_code_feedback(space_before=)", "maybe_code_feedback(before=)")
    if (missing(before)) {
      before <- if (isTRUE(space_before)) " " else ""
    }
  }
  if (is_present(space_after)) {
    deprecate_warn("0.2.3", "maybe_code_feedback(space_after=)", "maybe_code_feedback(after=)")
    if (missing(after)) {
      after <- if (isTRUE(space_after)) " " else ""
    }
  }

  # ensure before and after are single strings
  checkmate::check_character(before, any.missing = FALSE, null.ok = TRUE)
  checkmate::check_character(after, any.missing = FALSE, null.ok = TRUE)
  before <- paste(before, collapse = "\n")
  after  <- paste(after, collapse = "\n")

  # if an error occurs, return the default value
  # if no differences are found, return the default value
  # if any difference is found, maybe add space before and after
  capture_errors(
    {
      code_feedback_val <- code_feedback(
        user_code = user_code,
        solution_code = solution_code,
        solution_code_all = solution_code_all,
        env = env,
        allow_partial_matching = allow_partial_matching
      )
      if (is.null(code_feedback_val)) {
        return(default)
      }
      # return upgraded value
      paste0(before, code_feedback_val, after)
    },
    on_error = function(e, that_env) {
      # something bad happened. Return default value
      rlang::return_from(that_env, default)
    }
  )
}

#' @describeIn code_feedback Appends [maybe_code_feedback()] to the
#'   message generated by incorrect grades.
#'
#' @param expr A grading function — like [grade_this()] or [grade_result()] —
#'   or a character string. The code feedback will be appended to the message
#'   of any incorrect grades using [maybe_code_feedback()], set to always
#'   include the code feedback, if possible. If `expr` is a character string,
#'   `"{maybe_code_feedback()}"` is pasted into the string, without
#'   customization.
#' @param location Should the code feedback message be added before or after?
#'
#' @export
give_code_feedback <- function(
  expr,
  ...,
  env = parent.frame(),
  location = c("after", "before")
) {
  location <- match.arg(location)

  # evaluate expression in gradethis context to catch any grades
  # and also turn off maybe_code_feedback so that feedback isn't repeated twice
  expr_q <- rlang::get_expr(rlang::enquo(expr))
  res <- with_maybe_code_feedback(
    FALSE,
    eval_gradethis(rlang::eval_bare(expr_q, env))
  )

  # then dispatch on input type internally
  give_code_feedback_(res, env = env, location = location, ...)
}

give_code_feedback_ <- function(
  x,
  ...,
  env = parent.frame(),
  location = c("after", "before")
) {
  UseMethod("give_code_feedback_", x)
}

#' @export
give_code_feedback_.character <- function(x, ..., location = "after") {
  # This just inlines maybe_code_feedback() but doesn't guarantee it will show up
  mcf <- "{maybe_code_feedback()}"
  before <- identical(location, "before")
  paste0(if (before) mcf, x, if (!before) mcf)
}

#' @export
give_code_feedback_.function <- function(x, ..., env = NULL, location = "after") {
  function(check_env) {
    # get original grade without any code feedback (it will always be appended)
    grade <- capture_graded(with_maybe_code_feedback(FALSE, x(check_env)))

    give_code_feedback_(grade, env = check_env, location = location, ...)
  }
}

#' @export
give_code_feedback_.gradethis_graded <- function(
  grade,
  ...,
  env = rlang::env_parent(n = 2),
  location = "after"
) {
  solution_code <- get0(".solution_code", envir = env, ifnotfound = NULL)
  user_code <- get0(".user_code", envir = env, ifnotfound = NULL)

  if (is.null(solution_code) || !identical(grade$correct, FALSE)) {
    signal_grade(grade)
  }

  # What about correct grades with differences??

  feedback <- with_maybe_code_feedback(
    TRUE,
    maybe_code_feedback(user_code, solution_code, ...)
  )

  # If there isn't any feedback or if the feedback message has already been
  # added to the grade message, then just re-throw the grade
  if (identical(feedback, "") || grepl(feedback, grade$message, fixed = TRUE)) {
    signal_grade(grade)
  }

  before <- identical(location, "before")
  grade$message <- paste0(
    if (before) feedback,
    grade$message,
    if (!before) feedback
  )

  signal_grade(grade)
}

#' @export
give_code_feedback_.NULL <- function(x, ...) {
  invisible(NULL)
}

#' @export
give_code_feedback_.default <- function(x, ...) {
  stop(
    "give_code_feedback() expected a character, function, or grade.",
    call. = FALSE
  )
}
