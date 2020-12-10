

#' Grade student code against a solution
#'
#' Checks the code expression or the code result against a solution.
#'
#' `grade_this_code()` compares student code to a solution (i.e. model code) and
#' describes the first way that the student code differs. If the student code
#' exactly matches the solution, `grade_this_code()` returns a customizable success
#' message (`correct`). If the student code does not match the solution, a
#' customizable incorrect message (`incorrect`) can also be provided.
#'
#' `grade_this_code()` only inspects for code differences between the student's
#' code and the solution code. The final result of the student code and solution
#' code is ignored. See [code_feedback()] for implementation details on how code
#' is determined to be different.
#'
#' You can provide solution code for `grade_this_code()` to use in two ways:
#'
#' 1. Pass code as a character string or a quoted expression to the solution
#' argument of `grade_code()`
#'
#' 2. Make a "-solution" code chunk for the exercise to be checked in a learnr
#' document. There is no need to supply a solution argument for `grade_code()`
#' if you call it from the "-check" chunk of the same exercise. Likewise, there
#' is no need to supply a student submitted code argument when you call
#' `grade_code()` from a learnr document (learnr will provide the code that the
#' student submits when it runs `grade_code()`.
#'
#' For best results, name all arguments provided in the solution code.
#'
#' @param correct A `glue`-able character string to display if the student answer matches a
#'   known correct answer.
#'
#' @param incorrect A `glue`-able character string to display if the student answer matches
#'   a known incorrect answer. `.message` is available in the calling environment.
#' @param ... Ignored
#' @inheritParams code_feedback
#' @inheritParams grade_this
#'
# ' @param glue_pipe A glue string that returns the final message displayed when
# '   the student uses a pipe, `%>%`. Defaults to
# '   `getOption("gradethis_glue_pipe")`.
#'
#' @return a function whose first parameter should be an environment that contains
#' all necessary information to compare the code.  The result of the returned function will be a [graded()] object.
#' @seealso [code_feedback()], [grade_this()]
#' @examples
#'
#' # These are manual examples, see grading demo for `learnr` tutorial usage
#'
#' grade_this_code()(list(
#'   .user_code = "sqrt(log(2))",
#'   .solution_code = "sqrt(log(1))"
#' ))
#'
#' grade_this_code()(list(
#'   .user_code = rlang::quo(sqrt(log(2))),
#'   .solution_code = rlang::quo(sqrt(log(1)))
#' ))
#'
#' # Remember, only `grade_this_code(correct, incorrect)` should be used.
#' # The followup `list()`` and values will be called by `grade_learnr()`.
#' # To learn more about using `grade_this_code()` with learnr, see:
#' \dontrun{gradethis_demo()}
#' @export
grade_this_code <- function(
  correct = getOption("gradethis.code.correct", getOption("gradethis.pass", "Correct!")),
  incorrect = getOption("gradethis.code.incorrect", getOption("gradethis.fail", "Incorrect")),
  ...,
  allow_partial_matching = getOption("gradethis.code.partial_matching", TRUE),
  fail_code_feedback = getOption("gradethis.code.feedback", TRUE)
) {
  ellipsis::check_dots_empty()

  # MUST wrap calling function to be able to shim in `.correct`/`.incorrect`
  function(checking_env) {
    checking_env[[".__correct"]] <- correct
    checking_env[[".__incorrect"]] <- incorrect

    grade_this(
      fail_code_feedback = fail_code_feedback,
      expr = {
        # create variable `.message` for glue to find
        .message <- code_feedback(allow_partial_matching = allow_partial_matching)
        # call `pass`/`fail` inside `grade_this` to have access to `checking_env`
        if (is.null(.message)) {
          # need to use `get()` to avoid using `utils::globalVariables`
          pass(get(".__correct"))
        } else {
          fail(get(".__incorrect"))
        }
      }
    )(checking_env)
  }
}



#' Code Feedback
#'
#' Generate a message describing the first instance of a code mismatch. This function is built to be used within [grade_this()] without using arguments.
#' Manual calling of `code_feedback()` is also encouraged!
#'
#' @section Code differences:
#'
#' There are many different ways that code can be different, yet still the same. Here is how we detect code differences:
#'
#' 1. If the single values are different. Ex: `log(2)` vs `log(3)`
#' 2. If the function call is different. Ex: `log(2)` vs `sqrt(2)`
#' 3. Validate the user code can be standardised via [rlang::call_standardise()]. The `env` parameter is important for this step as \pkg{gradethis} does not readily know about user defined functions. Ex: `read.csv("file.csv")` turns into `read.csv(file = "file.csv")`
#' 4. If multiple formals are matched. Ex: `read.csv(f = "file.csv")` has `f` match to `file` and `fill`.
#' 5. Verify that every named argument in the solution appears in the user code. Ex: If the solution is `read.csv("file.csv", header = TRUE)`, `header` must exist.
#' 6. Verify that the user did not supply extra named arguments to `...`. Ex: `mean(x = 1:10, na.rm = TRUE)` vs `mean(x = 1:10)`
#' 7. Verify that every named argument in the solution matches the value of the corresponding user argument. Ex: `read.csv("file.csv", header = TRUE)` vs `read.csv("file.csv", header = FALSE)`
#' 8. Verify that the remaining aruguments of the user and solution code match in order and value. Ex: `mean(1:10, 0.1)` vs `mean(1:10, 0.2)`
#'
#' @param user_code String containing user code. Defaults to retrieving `.user_code` from the calling environment. (Required)
#' @param solution_code String containing solution code. Defaults to retrieving `.solution_code` from the calling environment. (Required)
#' @param env Environment used to standardise formals of the user and solution code. Defaults to retrieving `.envir_prep` from the calling environment. If not found, the [parent.frame()] will be used
#' @param ... Ignored
#' @param allow_partial_matching A logical if `FALSE` don't allow partial matching
#' @return If no discrepencies are found, a `NULL` value is returned. If a code difference is found, a character value describing the difference.
#' @describeIn code_feedback Determine code feedback
#' @export
#' @examples
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
#' code_feedback("read.csv('file.csv', header = FALSE)", "read.csv('file.csv', header = TRUE)")
#'
#' # No extra arguments are provided
#' code_feedback("mean(1:10)", "mean(1:10, na.rm = TRUE)")
#'
#' # Unstandardised arguments match in order and value
#' code_feedback("mean(1:10, 0.1)", "mean(1:10, 0.2)")
code_feedback <- function(
  user_code = get0(".user_code", parent.frame()),
  solution_code = get0(".solution_code", parent.frame()),
  env = get0(".envir_prep", parent.frame(), ifnotfound = parent.frame()),
  ...,
  allow_partial_matching = getOption("gradethis.code.partial_matching", TRUE)
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
    rlang::get_expr(x)
  } else {
    checkmate::assert_character(x, null.ok = FALSE, min.len = 1L, .var.name = name)
    str2expression(x)
  }
}


should_display_code_feedback <- function() {
  isTRUE(getOption("gradethis.code.feedback", FALSE))
}
with_code_feedback <- function(val, expr) {
  with_options(
    list("gradethis.code.feedback" = val),
    expr
  )
}

#' @describeIn code_feedback Return `code_feedback()` result when possible. Useful when setting default [fail()] glue messages. For example, if there is no solution, no code feedback will be given.
#' @param ... Ignored
#' @param default Default value to return if no code feedback is found or code feedback can be provided
#' @param space_before,space_after Logical value to determine if a space should be included before ([TRUE]) or after ([FALSE])
#' @export
maybe_code_feedback <- function(
  user_code = get0(".user_code", parent.frame()),
  solution_code = get0(".solution_code", parent.frame()),
  env = get0(".envir_prep", parent.frame(), ifnotfound = parent.frame()),
  ...,
  allow_partial_matching = getOption("gradethis.code.partial_matching", TRUE),
  default = "",
  space_before = TRUE,
  space_after = FALSE
) {
  ellipsis::check_dots_empty()

  # if feedback is not enabled, return
  if (!should_display_code_feedback()) {
    return(default)
  }

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
      paste0(
        if (isTRUE(space_before)) " ",
        code_feedback_val,
        if (isTRUE(space_after)) " "
      )
    },
    on_error = function(e, that_env) {
      # something bad happened. Return default value
      rlang::return_from(that_env, default)
    }
  )
}
