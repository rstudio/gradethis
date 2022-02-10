
#' Grade student code against a solution
#'
#' @description
#' `grade_this_code()` compares student code to a solution (i.e. model code) and
#' describes the first way in which the student code differs. If the student
#' code exactly matches the solution, `grade_this_code()` returns a customizable
#' success message (`correct`). If the student code does not match the solution,
#' a customizable incorrect message (`incorrect`) can also be provided.
#'
#' In most cases, to use `grade_this_code()`, ensure that your exercise has a
#' `-solution` chunk:
#'
#' ````
#' ```{r example-solution}
#' sqrt(log(1))
#' ```
#' ````
#'
#' Then, call `grade_this_code()` in your exercise's `-check` or `-code-check` chunk:
#'
#' ````
#' ```{r example-check}
#' grade_this_code()
#' ```
#' ````
#'
#' If `grade_this_code()` is called in a `-code-check` chunk and returns
#' feedback, either passing or failing feedback, then the user's code is not
#' executed. If you want the user to see the output of their code, call
#' `grade_this_code()` in the `-check` chunk. You can also use
#' `grade_this_code()` as a pre-check to avoid running code when it fails or
#' passes by calling `grade_this_code()` inside the `-code-check` chunk and
#' setting `action = "pass"` or `action = "fail"` to only return feedback when
#' the user's code passes or fails, respectively. (Note: requires \pkg{learnr}
#' version 0.10.1.9017 or later.)
#'
#' Learn more about how to use `grade_this_code()` in the **Details** section
#' below.
#'
#' @section Details:
#'
#'   `grade_this_code()` only inspects for code differences between the
#'   student's code and the solution code. The final result of the student code
#'   and solution code is ignored. See the **Code differences** section of
#'   [code_feedback()] for implementation details on how code is determined to
#'   be different.
#'
#'   You can call `grade_this_code()` in two ways:
#'
#'   1. If you want to check the student's code without evaluating it, call
#'      `grade_this_code()` in the `*-code-check` chunk.
#'
#'   1. To return grading feedback in along with the resulting output of the
#'      student's code, call `grade_this_code()` in the `*-check` chunk of the
#'      exercise.
#'
#'   To provide the solution code, include a `*-solution` code chunk in the
#'   learnr document for the exercise to be checked. When used in this way,
#'   `grade_this_code()` will automatically find and use the student's
#'   submitted code — `.user_code` in [grade_this()] — as well as the solution
#'   code — `.solution_code` in [grade_this()].
#'
#' @section Custom messages:
#'
#'   You can customize the `correct` and `incorrect` messages shown to the user
#'   by `grade_this_code()`. Both arguments accept template strings that are
#'   processed by [glue::glue()]. If you provide a custom template string, it
#'   completely overwrites the default string, but you can include the
#'   components used by the default message by adding them to your custom
#'   message.
#'
#'   There are four helper functions used in the default messages that you may
#'   want to include in your custom messages. To use the output of any of the
#'   following, include them inside braces in the template string. For example
#'   use `{code_feedback()}` to add the code feedback to your custom `incorrect`
#'   message.
#'
#'   1. [code_feedback()]: Adds feedback about the first observed difference
#'      between the student's submitted code and the model solution code.
#'
#'   2. [pipe_warning()]: Informs the user that their code was unpiped prior to
#'      comparison. This message is included by default to help clarify cases
#'      when the code feedback makes more sense in the unpiped context.
#'
#'   3. [random_praise()] and [random_encouragement()]: These praising and
#'      encouraging messages are included by default in correct and incorrect
#'      grades, by default.
#'
#' @examples
#' # For an interactive example run: gradethis_demo()
#'
#' # # These are manual examples, see grading demo for `learnr` tutorial usage
#'
#' grade_this_code()(
#'   mock_this_exercise(
#'     .user_code     = "sqrt(log(2))", # user submitted code
#'     .solution_code = "sqrt(log(1))"  # from -solution chunk
#'   )
#' )
#'
#' grade_this_code()(
#'   mock_this_exercise(
#'     # user submitted code
#'     .user_code     = "runif(1, 0, 10)",
#'     # from -solution chunk
#'     .solution_code = "runif(n = 1, min = 0, max = 1)"
#'   )
#' )
#'
#' # By default, grade_this_code() informs the user that piped code is unpiped
#' # when comparing to the solution
#' grade_this_code()(
#'   mock_this_exercise(
#'     # user submitted code
#'     .user_code     = "storms %>% select(year, month, hour)",
#'     # from -solution chunk
#'     .solution_code = "storms %>% select(year, month, day)"
#'   )
#' )
#'
#' # By setting `correct` or `incorrect` you can change the default message
#' grade_this_code(
#'   correct = "Good work!",
#'   incorrect = "Not quite. {code_feedback()} {random_encouragement()}"
#' )(
#'   mock_this_exercise(
#'     # user submitted code
#'     .user_code     = "storms %>% select(year, month, hour)",
#'     # from -solution chunk
#'     .solution_code = "storms %>% select(year, month, day)"
#'   )
#' )
#'
#' @param correct A `glue`-able character string to display if the student
#'   answer matches a known correct answer.
#' @param incorrect A `glue`-able character string to display if the student
#'   answer does not match the known correct answer. Use `code_feedback()` in
#'   this string to control the placement of the auto-generated feedback message
#'   produced by comparing the student's submission with the solution.
#' @param ... Ignored
#' @param action The action to take:
#'
#'   1. `"pass"` provide passing `correct` feedback when the user's code
#'      matches the solution code.
#'   2. `"fail"` provide failing `incorrect` feedback when the user's code does
#'      not match the solution code.
#'   3. `"both"` always provide passing or failing feedback.
#' @inheritParams code_feedback
#'
#' @return Returns a function whose first parameter will be an environment
#'   containing objects specific to the exercise and submission (see **Available
#'   variables**). For local testing, you can create a version of the expected
#'   environment for a mock exercise submission with [mock_this_exercise()].
#'   Calling the returned function on the exercise-checking environment will
#'   evaluate the grade-checking `expr` and return a final grade via [graded()].
#'
#' @seealso [code_feedback()], [grade_this()], [mock_this_exercise()]
#' @export
grade_this_code <- function(
  correct = getOption("gradethis.code_correct", getOption("gradethis.pass", "Correct!")),
  incorrect = getOption("gradethis.code_incorrect", getOption("gradethis.fail", "Incorrect")),
  ...,
  allow_partial_matching = getOption("gradethis.allow_partial_matching", TRUE),
  action = c("both", "pass", "fail")
) {
  ellipsis::check_dots_empty()

  action <- tolower(action)
  action <- match.arg(action)
  if (identical(action, "both")) {
    action <- c("pass", "fail")
  }

  # MUST wrap calling function to be able to shim in `.correct`/`.incorrect`
  function(check_env) {
    check_env[[".user_code"]] <- assert_user_code(.user_code, check_env)

    check_env[[".__correct"]] <- correct
    check_env[[".__incorrect"]] <- incorrect
    check_env[[".__action"]] <- action

    grade <- with_options(
      list(
        # Pass allow_partial_matching to internal code_feedback() calls
        gradethis.allow_partial_matching = allow_partial_matching,
        # The point of grade_this_code() is to return code feedback so we set
        # maybe_code_feedback to TRUE in case the user calls maybe_code_feedback()
        gradethis.maybe_code_feedback = TRUE
      ),
      grade_this({
        # check code for mistakes and store error feedback in .message so it
        # can be found by glue in fail(). Will be NULL if code is correct.
        .message <- code_feedback()

        # call `pass`/`fail` inside `grade_this` to have access to `check_env`
        # but need to use `get()` to avoid using `utils::globalVariables`
        if (is.null(.message)) {
          # no code_feedback() message means the code is correct
          if ("pass" %in% get(".__action")) {
            pass(get(".__correct"))
          }
        } else {
          if ("fail" %in% get(".__action")) {
            fail(get(".__incorrect"))
          }
        }

        invisible(NULL)
      })(check_env)
    )

    if (is.null(grade)) {
      return(invisible(NULL))
    }

    class(grade) <- c("gradethis_graded_this_code", class(grade))
    grade
  }
}
