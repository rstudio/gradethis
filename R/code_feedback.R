
#' Code Feedback
#'
#' Generate a message describing the first instance of a code mismatch. This
#' function is built to be used within [grade_this()] without using arguments.
#' Manual calling of `code_feedback()` is also encouraged!
#' 
#' @section Code differences:
#'
#' There are many different ways that code can be different, yet still the same.
#' Here is how we detect code differences:
#'
#' 1. If the single values are different. Ex: `log(2)` vs `log(3)`
#' 2. If the function call is different. Ex: `log(2)` vs `sqrt(2)`
#' 3. Validate the user code can be standardised via 
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
#' # Values are same
#' code_feedback("log(2)", "log(2)") # NULL # no differences found
#'
#' # Functions are different
#' code_feedback("log(2)", "sqrt(2)")
#'
#' # Standardise user names
#' code_feedback("read.csv('file.csv')", "read.csv(file = 'file.csv')") # NULL
#'
#' # No partial matching
#' code_feedback("read.csv(f = 'file.csv')", "read.csv(file = 'file.csv')")
#'
#' # All named arguments are provided (even if they match default value)
#' code_feedback("read.csv('file.csv')", "read.csv('file.csv', header = TRUE)")
#'
#' # All named argument values match
#' code_feedback(
#'   "read.csv('file.csv', header = FALSE)", 
#'   "read.csv('file.csv', header = TRUE)"
#' )
#'
#' # No extra arguments are provided
#' code_feedback("mean(1:10)", "mean(1:10, na.rm = TRUE)")
#'
#' # Unstandardised arguments match in order and value
#' code_feedback("mean(1:10, 0.1)", "mean(1:10, 0.2)")
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
#' # ```{r example-check}
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
#' # ```{r example-check}
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
#' # ```{r example-check}
#'   grade_result(
#'     fail_if(~ round(.result, 0) != 2, "Not quite!")
#'   )
#' # ```
#' grader(submission_wrong)
#' 
#' # But you can use give_code_feedback() to append code feedback
#' grader <- 
#' # ```{r example-check}
#'   give_code_feedback(grade_result(
#'     fail_if(~ round(.result, 0) != 2, "Not quite!")
#'   ))
#' # ```
#' grader(submission_wrong)
#' 
#' # The default `grade_this_code()` `incorrect` message adds code feedback,
#' # so be sure to remove \"{maybe_code_feedback()}\" from the incorrect message
#' grader <- 
#' # ```{r example-check}
#'   give_code_feedback(grade_this_code(incorrect = "{random_encouragement()}"))
#' # ```
#' grader(submission_wrong)
#'
#' @param user_code String containing user code. Defaults to retrieving
#'   `.user_code` from the calling environment. (Required)
#' @param solution_code String containing solution code. Defaults to retrieving
#'   `.solution_code` from the calling environment. (Required)
#' @param env Environment used to standardise formals of the user and solution
#'   code. Defaults to retrieving `.envir_prep` from the calling environment. If
#'   not found, the [parent.frame()] will be used
#' @param ... Ignored in `code_feedback()` and `maybe_code_feedback()`. In
#'   `give_code_feedback()`, `...` are passed to `maybe_code_feedback()`.
#' @param allow_partial_matching A logical. If `FALSE`, the partial matching of
#'   argument names is not allowed and e.g. `runif(1, mi = 0)` will return a
#'   message indicating that the full formal name `min` should be used. The
#'   default is set via the `gradethis.allow_partial_matching` option, or by
#'   [gradethis_setup()].
#'
#' @return If no discrepancies are found, `code_feedback()` returns `NULL`. If a
#'   code difference is found, a character value describing the difference. For
#'   safe use in glue strings, use `maybe_code_feedback()`, which returns an
#'   empty string if no discrepancies are found. `give_code_feedback()` adds
#'   code feedback to the messages of [fail()] grades.
#'   
#' @describeIn code_feedback Determine code feedback
#' @export
code_feedback <- function(
  user_code = get0(".user_code", parent.frame()),
  solution_code = get0(".solution_code", parent.frame()),
  env = get0(".envir_prep", parent.frame(), ifnotfound = parent.frame()),
  ...,
  allow_partial_matching = getOption("gradethis.allow_partial_matching", TRUE)
) {
  ellipsis::check_dots_empty()

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
    checkmate::assert_character(x, null.ok = FALSE, min.len = 1L, .var.name = name)
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

#' @describeIn code_feedback Return `code_feedback()` result when possible.
#'   Useful when setting default [fail()] glue messages. For example, if there
#'   is no solution, no code feedback will be given.
#'   
#' @param default Default value to return if no code feedback is found or code
#'   feedback can be provided
#' @param before,after Strings to be added before or after the code feedback
#'   message to ensure the message is properly formatted in your feedback.
#' @param space_before,space_after Deprecated. Use `before` and `after`.
#'   
#' @export
maybe_code_feedback <- function(
  user_code = get0(".user_code", parent.frame()),
  solution_code = get0(".solution_code", parent.frame()),
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
    signalCondition(grade)
    return(grade)
  }
  
  # What about correct grades with differences??

  feedback <- with_maybe_code_feedback(
    TRUE,
    maybe_code_feedback(user_code, solution_code, ...)
  )
  
  if (identical(feedback, "")) return(grade)
  
  before <- identical(location, "before")
  grade$message <- paste0(
    if (before) feedback, 
    grade$message, 
    if (!before) feedback
  )
  
  signalCondition(grade)
  grade
}


#' @export
give_code_feedback_.default <- function(x, ...) {
  stop(
    "give_code_feedback() expected a character, function, or grade.",
    call. = FALSE
  )
}
