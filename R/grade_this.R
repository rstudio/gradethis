#' Grade expression
#'
#' @section Available variables:
#'
#' `grade_this()` allows instructors to create an expression to grade. Within the expression,
#' all `learnr` tutorial variables variables are available for inspection with a `.` prepended to the name:
#'
#' * `.label`: Label for exercise chunk
#' * `.solution_code`: Code provided within the "-solution" chunk for the exercise
#' * `.user_code`: R code submitted by the user
#' * `.check_code`: Code provided within the "-check" (or "-code-check") chunk for the exercise.
#' * `.envir_prep`: A copy of the R environment before the execution of the chunk.
#' * `.envir_result`: The R environment after the execution of the chunk.
#' * `.evaluate_result`: The return value from the `evaluate::evaluate` function.
#' * `.last_value The last value from evaluating the user's exercise submission.
#' In addition, gradethis has provided some extra variables:
#' * `.user`, `.result`: A direct copy of `.last_value` for friendlier naming
#' * `.solution`: When accessed, will be the result of evaluating the `.solution_code` in a child environment of `.envir_prep`
#'
#' As the instructor, you are free to use any logic to determine a student's grade as long as a [graded()] object is signaled.
#' The check code can also contain \pkg{testthat} expectation code. Failed \pkg{testthat} expectations will be turned into [fail()]ed grades
#' with the corresponding message.
#'
#' @param expr Expression to be evaluated. MUST either signal a grade via [pass()] or [fail()] like \pkg{gradethis} functions or throw an error (via \pkg{testthat} or [stop()]). Errors will be converted to [fail()] calls with the corresponding error message.
#' @param ... ignored
#' @param fail_code_feedback Logical that determines if code feedback should be appended to the default failure message. Can also use [maybe_code_feedback()] in your custom [fail()] glue message.
#' @return a function whose first parameter should be an environment that contains all necessary information to execute the expression.  The evaluation of the expression should return a [graded()] object.
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
#'   .last_value = 4
#' ))
#'
#' grade_this({
#'   testthat::expect_type(.result, "integer") # will `fail()` if not an integer
#'   testthat::expect_equal(.result, 5L)        # will `fail()` if not equal to 5
#'   pass() # returns default message
#' })(list(
#'   .last_value = 5L
#' ))
#'
#' grade_this({
#'   testthat::expect_true(is.function(.result))
#'   testthat::expect_equal(.result(1), 3)
#'   pass()
#' })(list(
#'   .last_value = function(x) {x + 2}
#' ))
#'
#' # Remember, only `grade_this(expr)` should be used.
#' # The followup `list()` and values will be called by `grade_learnr()`
#' # To learn more about using `grade_this()` with learnr, see:
#' \dontrun{gradethis_demo()}
grade_this <- function(
  expr,
  ...,
  fail_code_feedback = getOption("gradethis.code.feedback", TRUE)
) {
  express <- rlang::get_expr(rlang::enquo(expr))

  function(checking_env) {

    # make sure fail calls can get code feed back (or not) if they want
    with_code_feedback(
      fail_code_feedback,

      # capture all pass/fail calls and errors thrown
      eval_gradethis({

        # force the evaluation of the expression in an environment
        rlang::eval_tidy(
          express,
          env = checking_env
        )
      })
    )
  }
}
