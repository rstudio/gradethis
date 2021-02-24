

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
#' @param correct A `glue`-able character string to display if the student 
#'   answer matches a known correct answer.
#'
#' @param incorrect A `glue`-able character string to display if the student
#'   answer does not match the known correct answer. Use `code_feedback()` in 
#'   this string to control the placement of the auto-generated feedback message
#'   produced by comparing the student's submission with the solution.
#' @param ... Ignored
#' @inheritParams code_feedback
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
#' # The followup `list()` and values will be called by 
#' # `gradethis_exercise_checker()`.
#' # To learn more about using `grade_this_code()` with learnr, see:
#' \dontrun{gradethis_demo()}
#' @export
grade_this_code <- function(
  correct = getOption("gradethis.code_correct", getOption("gradethis.pass", "Correct!")),
  incorrect = getOption("gradethis.code_incorrect", getOption("gradethis.fail", "Incorrect")),
  ...,
  allow_partial_matching = getOption("gradethis.allow_partial_matching", TRUE)
) {
  ellipsis::check_dots_empty()

  # MUST wrap calling function to be able to shim in `.correct`/`.incorrect`
  function(check_env) {
    check_env[[".__correct"]] <- correct
    check_env[[".__incorrect"]] <- incorrect

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
          pass(get(".__correct"))
        }
        
        fail(get(".__incorrect"))
      })(check_env)
    )
    class(grade) <- c("gradethis_graded_this_code", class(grade))
    grade
  }
}


