#' Grade expression
#'
#' @section Available variables:
#'
#'   `grade_this()` allows instructors to create an expression to grade. Within
#'   the expression, all `learnr` tutorial variables variables are available for
#'   inspection with a `.` prepended to the name:
#'
#'   * `.label`: Label for exercise chunk 
#'   * `.solution_code`: Code provided within the "-solution" chunk for the exercise 
#'   * `.user_code`: R code submitted by the user 
#'   * `.check_code`: Code provided within the "-check" (or "-code-check") chunk for the exercise. 
#'   * `.envir_prep`: A copy of the R environment before the execution of the chunk. 
#'   * `.envir_result`: The R environment after the execution of the chunk. 
#'   * `.evaluate_result`: The return value from the `evaluate::evaluate` function. 
#'   * `.last_value` The last value from evaluating the user's exercise submission. 
#'   
#'   In addition, \pkg{gradethis} has provided some extra variables: 
#'   
#'   * `.user`, `.result`: A direct copy of `.last_value` for friendlier naming 
#'   * `.solution`: When accessed, will be the result of evaluating the 
#'      `.solution_code` in a child environment of `.envir_prep`
#'
#'   As the instructor, you are free to use any logic to determine a student's
#'   grade as long as a [graded()] object is signaled. The check code can also
#'   contain \pkg{testthat} expectation code. Failed \pkg{testthat} expectations
#'   will be turned into [fail()]ed grades with the corresponding message.
#'
#' @param expr Expression to be evaluated. MUST either signal a grade via
#'   [pass()] or [fail()] like \pkg{gradethis} functions or throw an error (via
#'   \pkg{testthat} or [stop()]). Errors will be converted to [fail()] calls
#'   with the corresponding error message.
#' @param ... ignored
#' @param maybe_code_feedback Logical that determines if `maybe_code_feedback()`
#'   should provide code feedback when used in a [graded()] message. The default
#'   value can be set with [gradethis_setup()].
#'   
#'   Typically, [maybe_code_feedback()] is called in the default [fail()] 
#'   message (the default can be customized the `fail` argument of 
#'   [gradethis_setup()]). If the `maybe_code_feedback` argument is `FALSE`, 
#'   `maybe_code_feedback()` returns an empty string.
#'   
#' @return a function whose first parameter should be an environment that
#'   contains all necessary information to execute the expression.  The
#'   evaluation of the expression should return a [graded()] object.
#'   
#' @seealso [grade_this_code()], [eval_gradethis()]
#' @export
#' @examples
#'
#' # These are manual examples, see grading demo for `learnr` tutorial usage
#'
#' grade_this({
#'   fail_if_equal(4, "Try adding 1")
#'   pass_if_equal(5, "You got 5, great!")
#'   fail()
#' })(list(
#'   .result = 4
#' ))
#'
#' grade_this({
#'   testthat::expect_type(.result, "integer") # will `fail()` if not an integer
#'   testthat::expect_equal(.result, 5L)        # will `fail()` if not equal to 5
#'   pass() # returns default message
#' })(list(
#'   .result = 5L
#' ))
#'
#' grade_this({
#'   testthat::expect_true(is.function(.result))
#'   testthat::expect_equal(.result(1), 3)
#'   pass()
#' })(list(
#'   .result = function(x) {x + 2}
#' ))
#'
#' # Remember, only `grade_this(expr)` should be used.
#' # The followup `list()` and values will be called by 
#' # `gradethis_exercise_checker()`
#' # To learn more about using `grade_this()` with learnr, see:
#' \dontrun{gradethis_demo()}
grade_this <- function(
  expr,
  ...,
  maybe_code_feedback = getOption("gradethis.maybe_code_feedback", TRUE)
) {
  express <- rlang::get_expr(rlang::enquo(expr))
  
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
      check_env <- list2env(check_env)
    }
    
    check_env[[".__gradethis_check_env"]] <- TRUE

    # make sure fail calls can get code feed back (or not) if they want
    with_maybe_code_feedback(
      maybe_code_feedback,

      # capture all pass/fail calls and errors thrown
      eval_gradethis({

        # force the evaluation of the expression in an environment
        rlang::eval_bare(
          express,
          env = check_env
        )
      })
    )
  }
}

detect_grade_this <- function(env = parent.frame()) {
  # Is this running in a grade_this_check_env?
  get0(".__gradethis_check_env", envir = env, ifnotfound = FALSE)
}

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
#' 
#' @param check_env A grade checking environment. You can use
#'   [mock_this_exercise()] to prepare a mocked exercise submission
#'   environment. Otherwise, you don't need to use or set this argument.
#'   
#' @return Returns a neutral grade containing a message that includes any
#'   and all information available about the exercise and the current 
#'   submission. This information is all available for use within
#'   [grade_this()].
#' 
#' @export
debug_this <- function(check_env = parent.frame()) {
  
  if (!exists(".result", check_env)) {
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
  
  get_check_env <- function(x, otherwise = paste0("<no ", x, ">")) {
    get0(x, envir = check_env, ifnotfound = otherwise)
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
      code_block(get_check_env(".result"))
    ),
    if (!is.null(solution_code)) tags$p(
      html("Solution (<code>.solution</code>):"),
      code_block(get_check_env(".solution"))
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
