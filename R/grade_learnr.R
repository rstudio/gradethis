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
#' @param solution_code Code provided within the “-solution” chunk for the
#'   exercise.
#' @param user_code R code submitted by the user
#' @param check_code Code provided within the “-check” chunk for the exercise.
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

  # Sometimes no user code is provided, but
  # that means there is nothing to check. Also,
  # you do not want to parse NULL
  if (is.null(user_code)) {
    return(list(
      message = "I didn't receive your code. Did you write any?",
      correct = FALSE,
      type = "error",
      location = "append"
    ))
  } else {
    user_code <- parse(text = user_code)
    if (length(user_code) == 0) {
      return(list(
        message = "I didn't receive your code. Did you write any?",
        correct = FALSE,
        type = "error",
        location = "append"
      ))
    }
  }

  # Sometimes no solution is provided, but that
  # means there is nothing to check against. Also,
  # you do not want to parse NULL
  if (!is.null(solution_code)) {
    solution_code <- parse(text = solution_code)
    if (length(solution_code) == 0) {
      return(list(
        message = "No solution is provided for this exercise.",
        correct = TRUE,
        type = "info",
        location = "append"
      ))
    }
  }

  had_error_checking <- FALSE
  checked_result <- tryCatch(
    { # nolint
      # Run checking code to get feedback
      parsed_check_code <- parse(text = check_code)
      if (length(parsed_check_code) > 1) {
        # don't eval the last one to avoid bad check calls
        for (i in 1:(length(parsed_check_code) - 1)) {
          eval(parsed_check_code[[i]], envir_prep)
        }
      }
      grading_code <- rlang::call_standardise(parsed_check_code[[length(parsed_check_code)]],
                                             envir_prep)

      # get all grader args
      grader_args <- list(
        user_quo = rlang::as_quosure(user_code[[length(user_code)]], envir_result)
      )

      if (!is.null(solution_code)) {
        grader_args$solution_quo <- rlang::as_quosure(solution_code[[length(solution_code)]],
                                                      envir_prep)
      }

      # copy in all learnr arguments
      learnr_args <- list(...)
      learnr_args$label <- label
      learnr_args$solution_code <- solution_code
      learnr_args$user_code <- user_code
      learnr_args$check_code <- check_code
      learnr_args$envir_result <- envir_result
      learnr_args$evaluate_result <- evaluate_result
      learnr_args$envir_prep <- envir_prep
      learnr_args$last_value <- last_value

      # copy in all grader arguments
      grading_code$grader_args <- grader_args
      grading_code$learnr_args <- learnr_args

      # set user answer for the environment to find
      envir_prep$ans <- grader_args$user

      # eval code in a copy of the chunk's prepped environment
      eval(grading_code, envir_prep)
    },
    error = function(e) {
      # prevent the error from being re-thrown
      message("", e)
      had_error_checking <<- TRUE
      graded(
        correct = FALSE,
        message = "Error occured while checking the submission"
      )
    }
  )

  if (!checkmate::test_class(checked_result, "grader_graded")) {
    stop("`grade_learnr` should receive a `graded` value from every `-check` chunk")
  }

  message_type <-
    if (had_error_checking) {
      "warning"
    } else {
      if (checked_result$correct) {
        "success"
      } else {
        "error"
      }
    }

  ret <- list(
    message = checked_result$message,
    correct = checked_result$correct,
    type = message_type,
    location = "append"
  )

  ret
}
