#' A checker function to use with `learnr`
#'
#' For exercise checking, `learnr` tutorials require a function that
#' `learnr` can use in the background to run the code in each "-check"
#' chunk and to format the results into a format that `learnr` can display.
#' The function must accept a specific set of inputs and return a specific type
#' of output (see [graded()]). Instructors are not intended to use the
#' `grade_learnr` function directly, but to pass it to the
#' `exercise.checker` knitr chunk option within the setup chunk of the
#' `learnr` tutorial.
#'
#' To enable exercise checking in your learnr tutorial, set
#' `tutorial_options(exercise.checker = grade_learnr)` in the setup chunk
#' of your tutorial.
#'
#' Run `gradethis_demo()` to see an example learnr document that uses
#' `grade_learnr()`.
#'
#' @param label Label for exercise chunk
#' @param solution_code Code provided within the "-solution" chunk for the
#'   exercise.
#' @param user_code R code submitted by the user
#' @param check_code Code provided within the "-check" (or "-code-check") chunk for the exercise.
#' @param envir_result The R environment after the execution of the chunk.
#' @param evaluate_result The return value from the `evaluate::evaluate`
#'   function.
#' @param envir_prep A copy of the R environment before the execution of the
#'   chunk.
#' @param last_value The last value from evaluating the user's exercise submission.
#' @param ... Extra arguments supplied by learnr
#'
#' @return An R list which contains several fields indicating the result of the
#'   check.
#' @export
#' @seealso [gradethis_setup()]
#'
#' @examples
#' \dontrun{gradethis_demo()}
grade_learnr <- function(label = NULL,
                         solution_code = NULL,
                         user_code = NULL,
                         check_code = NULL,
                         envir_result = NULL,
                         evaluate_result = NULL,
                         envir_prep = NULL,
                         last_value = NULL,
                         ...) {
  # Call this function in such a way that it can use other gradethis internals when called by learnr
  # (i.e., make tutorial_options(exercise.checker = gradethis::grade_learnr) always work)
  utils::getFromNamespace("grade_learnr_", "gradethis")(
    label = label,
    solution_code = solution_code,
    user_code = user_code,
    check_code = check_code,
    envir_result = envir_result,
    evaluate_result = evaluate_result,
    envir_prep = envir_prep,
    last_value = last_value,
    ...
  )
}


grade_learnr_ <- function(
  label = NULL,
  solution_code = NULL,
  user_code = NULL,
  check_code = NULL,
  envir_result = NULL,
  evaluate_result = NULL,
  envir_prep = NULL,
  last_value = NULL,
  ...
) {

  learnr_args <- list(
    label = label,
    solution_code = solution_code,
    user_code = user_code,
    check_code = check_code,
    envir_result = envir_result,
    evaluate_result = evaluate_result,
    envir_prep = envir_prep,
    last_value = last_value,
    ...
  )

  user_code <- tryCatch(
    parse(text = user_code %||% ""),
    error = function(e) {
      parse_checker <- getOption(
        "exercise.parse.error",
        function(...) {
          fail(paste(
            "It looks like this might not be valid R code:",
            conditionMessage(e),
            "\nCheck that you have closed every \", ', (, and { ",
            "with a matching \", ', ), and }. Also look for missing ",
            "commas. R cannot determine how to turn your text into ",
            "a complete command."
          ))
        }
      )
      # TODO? check that parse_checker is a function with proper args
      eval_gradethis({
        do.call(parse_checker, append(list(parse_error = e), learnr_args))
      })
    }
  )

  if (is_graded(user_code)) {
    user_code <- feedback(user_code)
  }
  if (is_feedback(user_code)) {
    return(user_code)
  }
  if (length(user_code) == 0) {
    return(feedback(
      fail("I didn't receive your code. Did you write any?"),
      type = "info"
    ))
  }

  check_label <-
    if (is.null(envir_result)) {
      paste0(label, "-code-check")
    } else {
      paste0(label, "-check")
    }

  ## setup an environment for checking
  # envir for function call
  chunk_envir <- learnr::duplicate_env(envir_prep)
  # object to pass to checking function that should contain all of the information
  check_obj_envir <- new.env(parent = chunk_envir)

  # Copy over all learnr args into the checking environment
  for (name in names(learnr_args)) {
    check_obj_envir[[paste0(".", name)]] <- learnr_args[[name]]
  }

  # Add gradethis specific check objects
  check_obj_envir[[".result"]] <- last_value
  check_obj_envir[[".user"]] <- last_value
  delayedAssign(
    assign.env = check_obj_envir,
    x = ".solution",
    {
      # Delayed evaluation of `.solution!`
      solution_expr <- parse(text = solution_code)
      if (length(solution_expr) == 0) {
        rlang::return_from(checking_envir, feedback(
          fail("No solution is provided for this exercise."),
          type = "info"
        ))
      } else {
        # solution code exists...
        # Using eval_tidy does not evaluate the expression. Using eval() instead
        eval(
          solution_expr,
          envir = new.env(parent = envir_prep)
        )
      }
    }
  )

  # evaluate all checking code
  checking_envir <- rlang::current_env()
  to_check_fn <- capture_errors(
    {
      parsed_check_code <- parse(text = check_code %||% "")
      capture_graded(
        {
          eval(parsed_check_code, envir = chunk_envir)
        },
        # if a `pass()`/`fail()` is used in the regular check chunk with no user submission context, it should be an error
        on_graded = function(gc_obj, ignore) {
          fn_used <- if (isTRUE(gc_obj$correct)) "`pass()`" else "`fail()`"

          # notify author of their mistake
          message(
            "A ", fn_used, " statement was executed without access to student feedback. (Prematurely graded)\n",
            "Remember to only call ", fn_used, " inside your checking function (ex: `grade_this({})`"
          )
          # return from main function (even though in a inner function! voodoo!)
          rlang::return_from(checking_envir, feedback(
            fail("A problem occurred with your teacher's grading code. Defaulting to _incorrect_"),
            type = "error"
          ))
        }
      )
    },
    # if an unhandled error occurs while checking...
    on_error = function(e, ignore) {
      # notify author of their mistake
      message("Error while executing checking `", check_label, "` chunk: ", e)
      # return from main function (even though in a inner function! voodoo!)
      rlang::return_from(checking_envir, feedback(
        fail("A problem occurred with your teacher's grading code. Defaulting to _incorrect_"),
        type = "error"
      ))
    }
  )

  if (
    !(
      # make sure the returned value from check chunk evaluation is a function
      checkmate::test_function(to_check_fn) &&
      # ...that accepts at least 1 argument
      checkmate::test_number(length(formals(to_check_fn)), lower = 1)
    )
  ) {
    # notify author of their mistake
    message(
      "`", check_label, "` chunk did not return a function (such as `grade_this`) that accepts 1 argument containing the checking object",
      "\nObject returned:\n",
      paste0(
        utils::capture.output(
          utils::str(to_check_fn)
        ),
        collapse = "\n"
      )
    )
    return(
      feedback(
        fail("A problem occurred with your teacher's grading code. Defaulting to _incorrect_"),
        type = "error"
      )
    )
  }

  # evaluate the function with the check envir passed in. (Passing an environment allows for `.solution` to be calculated on demand)
  # evaluation should handle errors themselves, but wrap in eval_gradethis to be sure
  graded_result <- eval_gradethis(
    to_check_fn(check_obj_envir)
  )

  # make sure the result is a pass or fail
  if (!is_graded(graded_result)) {
    message("`", check_label, "` chunk did not mark an answer as correct or incorrect. Consider adding a `pass()` or `fail()` at the end of your `", check_label, "` code")
    return(
      feedback(
        fail("A problem occurred with your teacher's grading code. Defaulting to _incorrect_"),
        type = "error"
      )
    )
  }

  # return result like normal
  feedback(
    graded_result,
    type = "auto"
  )
}



#' Setup gradethis for use within learnr
#'
#' Call this function inside a learnr document's setup chunk in order to use
#' a suggested behavior when grading exercises.
#'
#' @inheritParams learnr::tutorial_options
#' @param ... arguments passed to [learnr::tutorial_options()]
#' @export
#' @seealso [grade_learnr()]
gradethis_setup <- function(
  exercise.timelimit = 60,
  exercise.checker = grade_learnr,
  exercise.error.check.code = "grade_code()",
  ...
) {
  require(gradethis)
  learnr::tutorial_options(
    exercise.timelimit = exercise.timelimit,
    exercise.checker = exercise.checker,
    exercise.error.check.code = exercise.error.check.code,
    ...
  )
}
