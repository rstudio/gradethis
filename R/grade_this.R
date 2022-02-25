#' Grade a student's submission using custom logic
#'
#' @description
#' `grade_this()` allows instructors to write custom logic to evaluate, grade
#' and give feedback to students. To use `grade_this()`, call it directly in
#' your `*-check` chunk:
#'
#' ````
#' ```{r example-check}
#' grade_this({
#'   # custom checking code appears here
#'   if (identical(.result, .solution)) {
#'     pass("Great work!")
#'   }
#'   fail("Try again!")
#' })
#' ```
#' ````
#'
#' `grade_this()` makes available a number of objects based on the exercise and
#' the student's submission that can be used to evaluate the student's submitted
#' code. See `?"grade_this-objects"` for more information about these objects.
#'
#' As the instructor, you are free to use any logic to determine a student's
#' grade as long as a [graded()] object is signaled. The check code can also
#' contain \pkg{testthat} expectation code. Failed \pkg{testthat} expectations
#' will be turned into [fail()]ed grades with the corresponding message.
#'
#' A final grade is signaled from `grade_this()` using the [graded()] helper
#' functions, which include [pass()], [fail()], among others. `grade_this()`
#' uses condition handling to short-circuit further evaluation when a grade is
#' reached. This means that you may also signal a failing grade using any of the
#' `expect_*()` functions from \pkg{testthat}, other functions designed to work
#' with \pkg{testthat}, such as \pkg{checkmate}, or standard R errors via
#' `stop()`. Learn more about this behavior in [graded()] in the section
#' **Return a grade immediately**.
#'
#' @examples
#' # For an interactive example run: gradethis_demo()
#'
#' # Suppose we have an exercise that prompts students to calculate the
#' # average height of Loblolly pine trees using the `Loblolly` data set.
#' # We might write an exercise `-check` chunk like the one below.
#' #
#' # Since grade_this() returns a function, we'll save the result of this
#' # "chunk" as `grader()`, which can be called on an exercise submission
#' # to evaluate the student's code, which we'll simulate with
#' # `mock_this_exercise()`.
#'
#' grader <-
#'   # ```{r example-check}
#'   grade_this({
#'     if (length(.result) != 1) {
#'       fail("I expected a single value instead of {length(.result)} values.")
#'     }
#'
#'     if (is.na(.result)) {
#'       fail("I expected a number, but your code returned a missing value.")
#'     }
#'
#'     avg_height <- mean(Loblolly$height)
#'     if (identical(.result, avg_height)) {
#'       pass("Great work! The average height is {round(avg_height, 2)}.")
#'     }
#'
#'     # Always end grade_this() with a default grade.
#'     # By default fail() will also give code feedback,
#'     # if a solution is available.
#'     fail()
#'   })
#' # ```
#'
#' # Simulate an incorrect answer: too many values...
#' grader(mock_this_exercise(.user_code = Loblolly$height[1:2]))
#'
#' # This student submission returns a missing value...
#' grader(mock_this_exercise(mean(Loblolly$Seed)))
#' # This student submission isn't caught by any specific tests,
#' # the final grade is determined by the default (last) value in grade_this()
#' grader(mock_this_exercise(mean(Loblolly$age)))
#'
#' # If you have a *-solution chunk,
#' # fail() without arguments gives code feedback...
#' grader(
#'   mock_this_exercise(
#'     .user_code = mean(Loblolly$age),
#'     .solution_code = mean(Loblolly$height)
#'   )
#' )
#'
#' # Finally, the "student" gets the correct answer!
#' grader(mock_this_exercise(mean(Loblolly$height)))
#' @param expr The grade-checking expression to be evaluated. This expression
#'   must either signal a grade via [pass()] or [fail()] functions or their
#'   sibling functions.
#'
#'   By default, errors in this expression are converted to "internal problem"
#'   grades that mask the error for the user. If your grading logic relies on
#'   unit-test-styled functions, such as those from \pkg{testthat}, you can use
#'   [fail_if_error()] to convert errors into [fail()] grades.
#' @param maybe_code_feedback Should `maybe_code_feedback()` provide code
#'   feedback when used in a [graded()] message? The default value can be set
#'   with [gradethis_setup()].
#'
#'   Typically, [maybe_code_feedback()] is called in the default [fail()]
#'   message (the default can be customized the `fail` argument of
#'   [gradethis_setup()]). If the `maybe_code_feedback` argument is `FALSE`,
#'   `maybe_code_feedback()` returns an empty string.
#' @param ... Ignored
#'
#' @return Returns a function whose first parameter will be an environment
#'   containing objects specific to the exercise and submission (see **Available
#'   variables**). For local testing, you can create a version of the expected
#'   environment for a mock exercise submission with [mock_this_exercise()].
#'   Calling the returned function on the exercise-checking environment will
#'   evaluate the grade-checking `expr` and return a final grade via [graded()].
#'
#' @seealso [grade_this_code()], [mock_this_exercise()], [gradethis_demo()]
#' @export
grade_this <- function(
  expr,
  ...,
  maybe_code_feedback = getOption("gradethis.maybe_code_feedback", TRUE)
) {
  expr_quo <- rlang::enquo(expr)
  expr_env <- rlang::quo_get_env(expr_quo)
  expr <- rlang::quo_get_expr(expr_quo)

  if ("fail_code_feedback" %in% names(list(...))) {
    lifecycle::deprecate_warn(
      "0.2.3",
      "grade_this(fail_code_feedback=)",
      "grade_this(maybe_code_feedback=)"
    )
    if (missing(maybe_code_feedback)) {
      maybe_code_feedback <- list(...)$fail_code_feedback
    }
  }

  function(check_env) {
    if (is.list(check_env)) {
      check_env <- list2env(check_env, envir = new.env(parent = emptyenv()))
    }

    check_env[[".__gradethis_check_env"]] <- TRUE

    # Ensure that check_env has expr_env as a parent
    #
    # +------------+       +-----------+
    # |  expr_env  +------>| check_env |
    # +------------+       +-----------+
    rlang::env_poke_parent(check_env, expr_env)

    # Turn the grading code into a function defined in the `check_env`
    do_grade_this <- rlang::new_function(NULL, body = expr, env = check_env)

    # make sure fail calls can get code feed back (or not) if they want
    with_maybe_code_feedback(
      maybe_code_feedback,

      # capture all pass/fail calls and errors thrown
      eval_gradethis(do_grade_this())
    )
  }
}

detect_grade_this <- function(env = parent.frame()) {
  # Is this running in a grade_this_check_env?
  get0(".__gradethis_check_env", envir = env, ifnotfound = FALSE)
}


# Sentinel Values ----
placeholder <- function(class, ...) {
  structure(list(), class = c(class, ..., "gradethis_placeholder"))
}

is_placeholder <- function(x, which = "gradethis_placeholder") {
  inherits(x, which)
}

assert_not_placeholder <- function(x) {
  if (is_placeholder(x)) {
    rlang::abort(glue::glue("Unable to find value for placeholder `{class(x)[1]}`"))
  }
}

# @export
print.gradethis_placeholder <- function(x, ...) {
  type <- class(x)[1]
  desc <- placeholder_definition(type)
  desc <- glue::glue("A placeholder for `{type}` for use in `grade_this()`. {desc}")
  cat(strwrap(desc), sep = "\n")
}

placeholder_definition <- function(x) {
  if (is_placeholder(x)) {
    x <- class(x)[1]
  }
  switch(
    x,
    .label = "The exercise label.",
    .engine = "The exercise engine, typically 'r'.",
    .last_value = ,
    .user = ,
    .result = "The last value returned from evaluating the user's exercise submission.",
    .user_code = "A string containing the code submitted by the user.",
    .solution = "The last value returned from evaluating the `.solution_code` for the exercise (evaluated in `.envir_prep`).",
    .solution_code = "A string containing the code provided within the `*-solution` chunk for the exercise.",
    .solution_code_all = "A list containing the code of all solutions when multiple solutions are provided in the `*-solution` chunk for the exercise. Solutions are separated by header comments, e.g. `# base_r ----`.",
    .check_code = "A string containing the code provided within the `*-check` or `*-code-check` chunk for the exercise.",
    .envir_prep = "A copy of the R environment after running the exercise setup code and before the execution of the student's submitted code.",
    .envir_result = "The R environment after running the student's submitted code.",
    .evaluate_result = "The return value from the [evaluate::evaluate()] function (see learnr's documentation).",
    .stage = "The current checking stage in the learnr exercise evaluation lifecycle: 'code_check', 'error_check', or 'check'",
    ""
  )
}

#' Checking environment objects for use in `grade_this()`
#'
#' @description
#' [grade_this()] allows instructors to determine a grade and to create custom
#' feedback messages using custom R code. To facilitate evaluating the
#' exercise, [grade_this()] makes available a number of objects that can be
#' referenced within the `{ ... }` expression.
#'
#' All of the objects provided by `learnr` to an exercise checking function
#' are available for inspection. To avoid name collisions with user or
#' instructor code, the names of these objects all start with `.`.
#'
#' * `.label`: `r placeholder_definition(".label")`
#' * `.engine`: `r placeholder_definition(".engine")`
#' * `.last_value`: `r placeholder_definition(".last_value")`
#' * `.solution_code`: `r placeholder_definition(".solution_code")`
#' * `.user_code`: `r placeholder_definition(".user_code")`
#' * `.check_code`: `r placeholder_definition(".check_code")`
#' * `.envir_prep`: `r placeholder_definition(".envir_prep")`
#' * `.envir_result`: `r placeholder_definition(".envir_result")`
#' * `.evaluate_result`: `r placeholder_definition(".evaluate_result")`
#' * `.stage`: `r placeholder_definition(".stage")`
#'
#' In addition, \pkg{gradethis} has provided some extra objects:
#'
#' * `.user`, `.result`: `r placeholder_definition(".user")`
#' * `.solution`: `r placeholder_definition(".solution")`
#' * `.solution_code_all`: `r placeholder_definition(".solution_code_all")`
#'
#' @name grade_this-objects
NULL

#' @rdname grade_this-objects
#' @export
.result <- placeholder(".result")

#' @rdname grade_this-objects
#' @export
.user <- placeholder(".user", ".result")

#' @rdname grade_this-objects
#' @export
.last_value <- placeholder(".last_value", ".result")

#' @rdname grade_this-objects
#' @export
.solution <- placeholder(".solution")

#' @rdname grade_this-objects
#' @export
.user_code <- placeholder(".user_code")

empty_code <- function(code, check_null = FALSE) {
  if (check_null) {
    empty_code <- is.null(code) || length(code) == 0 || !nzchar(code)
  } else {
    empty_code <- length(code) && !nzchar(code)
  }

  if (empty_code) {
    return(
      graded(
        logical(),
        "I didn't receive your code. Did you write any?",
        type = "info"
      )
    )
  }
}

#' @rdname grade_this-objects
#' @export
.solution_code <- placeholder(".solution_code")

#' @rdname grade_this-objects
#' @export
.solution_code_all <- placeholder(".solution_code_all")

#' @rdname grade_this-objects
#' @export
.envir_prep <- placeholder(".envir_prep")

#' @rdname grade_this-objects
#' @export
.envir_result <- placeholder(".envir_result")

#' @rdname grade_this-objects
#' @export
.evaluate_result <- placeholder(".evaluate_result")

#' @rdname grade_this-objects
#' @export
.label <- placeholder(".label")

#' @rdname grade_this-objects
#' @export
.stage <- placeholder(".stage")

#' @rdname grade_this-objects
#' @export
.engine <- placeholder(".engine")

#' Debug an exercise submission
#'
#' When used in a `*-check` chunk or inside [grade_this()], `debug_this()`
#' displays in the \pkg{learnr} tutorial a complete listing of the variables
#' and environment available for checking. This can be helpful when you need
#' to debug an exercise and a submission.
#'
#' @section Debugging exercises:
#'
#' ```{r child = "man/fragments/debug_this-usage-setup.Rmd"}
#' ```
#'
#' \if{html}{
#' The debug output will look like the following when used as described
#' below.
#'
#' \Sexpr[echo=FALSE,results=rd,stage=build]{
#' submission <- gradethis::mock_this_exercise("# user submits\nx + 2", "x + 3", setup_exercise = "x <- 1", .label = "example")
#' paste("\\\out{\n<blockquote>", gradethis::debug_this(submission)$message, "</blockquote>}", sep = "\n")
#' }
#' }
#'
#' ```{r child = "man/fragments/debug_this-usage.Rmd"}
#' ```
#'
#' @examples
#' # Suppose we have an exercise (guess the number 42). Mock a submission:
#' submission <- mock_this_exercise(.user_code = 40, .solution_code = 11 + 31)
#'
#' # Call `debug_this()` inside your *-check chunk, is equivalent to
#' debug_this()(submission)$message
#'
#' # The remaining examples produce equivalent output
#' \dontrun{
#' # Or you can call `debug_this()` inside a `grade_this()` call
#' # at the point where you want to get debug feedback.
#' grade_this({
#'   pass_if_equal(42, "Good stuff!")
#'
#'   # Find out why this is failing??
#'   debug_this()
#' })(submission)
#'
#' # Set default `fail()` message to show debug information
#' # (for tutorial development only!)
#' old_opts <- options(gradethis.fail = "{debug_this()}")
#'
#' grade_this({
#'   pass_if_equal(42, "Good stuff!")
#'
#'   fail()
#' })(submission)
#'
#' # default fail() will show debug until you reset gradethis.fail option
#' options(old_opts)
#' }
#'
#' @param check_env A grade checking environment. You can use
#'   [mock_this_exercise()] to prepare a mocked exercise submission
#'   environment. Otherwise, you don't need to use or set this argument.
#'
#' @return Returns a neutral grade containing a message that includes any
#'   and all information available about the exercise and the current
#'   submission. The output lets you visually explore the objects available for
#'   use within your [grade_this()] grading code.
#'
#' @export
debug_this <- function(check_env = parent.frame()) {

  if (!exists(".result", envir = check_env) || is_placeholder(get(".result", envir = check_env))) {
    # most likely called outside of grade_this(), so return
    # debug_this directly to be used as a checking function
    return(debug_this)
  }

  tags <- htmltools::tags
  html <- htmltools::HTML
  collapse <- function(...) paste(..., collapse = "\n")

  str_chr <- function(x) {
    utils::capture.output(utils::str(x))
  }

  str_env <- function(env) {
    vars <- ls(env)
    names(vars) <- vars
    x <- str_chr(lapply(vars, function(v) get(v, env)))
    x[-1]
  }

  code_block <- function(value, engine = "r") {
    tags$pre(
      class = engine,
      tags$code(collapse(value), .noWS = "inside"),
      .noWS = "inside"
    )
  }

  get_check_env <- function(x, otherwise = I(paste0("<no ", x, ">"))) {
    get0(x, envir = check_env, ifnotfound = otherwise)
  }

  prnt <- function(x) {
    if (is_AsIs(x)) return(x)
    utils::capture.output(print(x))
  }

  solution_code <- get_check_env(".solution_code")

  message <- htmltools::tagList(
    tags$p(
      tags$strong(html("Exercise label (<code>.label</code>):")),
      tags$code(get_check_env(".label")),
      tags$br(),
      tags$strong(html("Engine (<code>.engine</code>):")),
      tags$code(get_check_env(".engine"))
    ),
    tags$p(
      html("Submission (<code>.result</code>, <code>.user</code>, <code>.last_value</code>):"),
      code_block(prnt(get_check_env(".result")))
    ),
    if (!is.null(solution_code)) tags$p(
      html("Solution (<code>.solution</code>):"),
      code_block(prnt(get_check_env(".solution")))
    ),
    tags$details(
      tags$summary(tags$code(".envir_prep")),
      code_block(str_env(get_check_env(".envir_prep")), engine = NULL)
    ),
    tags$details(
      tags$summary(tags$code(".envir_result")),
      code_block(str_env(get_check_env(".envir_result")), engine = NULL)
    ),
    tags$details(
      tags$summary(tags$code(".user_code")),
      code_block(get_check_env(".user_code"), get_check_env(".engine", otherwise = "r"))
    ),
    tags$details(
      tags$summary(tags$code(".solution_code")),
      if (is.null(solution_code)) {
        "No solution."
      } else {
        code_block(solution_code)
      }
    ),
    if (!is.null(get_check_env(".error", NULL))) {
      tags$details(
        tags$summary(tags$code(".error")),
        code_block(get_check_env(".error"))
      )
    }
  )

  graded(logical(0), message = message, type = "custom", location = "replace")
}
