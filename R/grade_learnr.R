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
#' @param check_code Code provided within the "-check" chunk for the exercise.
#' @param envir_result The R environment after the execution of the chunk.
#' @param evaluate_result The return value from the `evaluate::evaluate`
#'   function.
#' @param envir_prep A copy of the R environment before the execution of the
#'   chunk.
#' @param last_value The last value from evaluating the exercise.
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


grade_learnr_ <- function(label = NULL,
                         solution_code = NULL,
                         user_code = NULL,
                         check_code = NULL,
                         envir_result = NULL,
                         evaluate_result = NULL,
                         envir_prep = NULL,
                         last_value = NULL,
                         ...) {

  learnr_args <- list(
    ..., label = label,
    solution_code = solution_code,
    user_code = user_code,
    check_code = check_code,
    envir_result = envir_result,
    evaluate_result = evaluate_result,
    envir_prep = envir_prep,
    last_value = last_value
  )

  user_code <- tryCatch(
    parse(text = user_code %||% ""),
    error = function(e) {
      parse_checker <- getOption(
        "exercise.parse.error",
        function(...) {
          legacy_graded(
            correct = FALSE,
            message = paste(
              "Uh oh, the R code produced a syntax error:",
              conditionMessage(e),
              "\nCheck that you have closed every \", ', (, and { ",
              "with a matching \", ', ), and }. Also look for missing ",
              "commas. R cannot determine how to turn your text into ",
              "a complete command."
            )
          )
        }
      )
      # check that parse_checker is a function with proper args
      do.call(parse_checker, list(parse_error = e, learnr_args = learnr_args))
    }
  )

  if (is_graded(user_code)) {
    user_code <- feedback(user_code)
  }
  if (is_feedback(user_code)) {
    return(user_code)
  }
  if (length(user_code) == 0) {
    return(feedback(legacy_graded(
        message = "I didn't receive your code. Did you write any?",
        correct = FALSE
    )))
  }

  # Sometimes no solution is provided, but that
  # means there is nothing to check against. Also,
  # you do not want to parse NULL
  if (!is.null(solution_code)) {
    solution_code <- parse(text = solution_code)
    if (length(solution_code) == 0) {
      grade <- legacy_graded(
        message = "No solution is provided for this exercise.",
        correct = TRUE
      )
      return(feedback(grade, type = "info"))
    }
  }

  had_error_checking <- FALSE
  checked_result <- tryCatch(
    { # nolint
      ignore_graded({
        # Run checking code to get feedback
        parsed_check_code <- parse(text = check_code %||% "")

        if (length(parsed_check_code) > 1) {
          # don't eval the last one to avoid bad check calls
          for (i in 1:(length(parsed_check_code) - 1)) {
            eval(parsed_check_code[[i]], envir_prep)
          }
        }
        grading_code <- rlang::call_standardise(
          parsed_check_code[[length(parsed_check_code)]],
          envir_prep
        )

        # get all grader args
        grader_args <- list(
          user_quo = rlang::as_quosure(user_code, envir_result)
        )

        if (!is.null(solution_code)) {
          grader_args$solution_quo <-
            rlang::as_quosure(solution_code, envir_prep)
        }

        # copy in all grader arguments
        grading_code$grader_args <- grader_args
        grading_code$learnr_args <- learnr_args

        # eval code in a copy of the chunk's prepped environment
        eval(grading_code, envir_prep)
      })
    },
    error = function(e) {
      # prevent the error from being re-thrown
      message("", e)
      had_error_checking <<- TRUE
      ignore_graded({
        fail("Error occurred while checking the submission")
      })
    }
  )

  if (!checkmate::test_class(checked_result, "gradethis_graded")) {
    stop("`grade_learnr` should receive a `graded` value from every `-check` chunk")
  }

  feedback(
    checked_result,
    type = if (had_error_checking) "warning" else "auto"
  )
}


#' @rdname grade_learnr
#' @export
grade_learnr_error <- function(solution_code = NULL, check_code = "grade_code()", ...) {
  if (is.null(solution_code)) {
    return(NULL)
  }
  utils::getFromNamespace("grade_learnr", "gradethis")(
    solution_code = solution_code, check_code = check_code, ...
  )
}

learnr_env <- function(learnr_args) {
  learnr_args$envir_result %||%
    learnr_args$envir_prep %||%
    stop("Internal error. learnr did not pass a relevant environment")
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
gradethis_setup <- function(exercise.timelimit = 60, exercise.checker = grade_learnr,
                            exercise.error.check.code = "grade_code()", ...) {
  require(gradethis)
  learnr::tutorial_options(
    exercise.timelimit = exercise.timelimit,
    exercise.checker = exercise.checker,
    exercise.error.check.code = exercise.error.check.code,
    ...
  )
}
