#' A checker function to use with \pkg{learnr}
#'
#' For exercise checking, \pkg{learnr} tutorials require a function that
#' \pkg{learnr} can use in the background to run the code in each "-check"
#' chunk and to format the results into a format that \pkg{learnr} can display.
#' To enable exercise checking in your learnr tutorial, attach \pkg{gradethis}
#' with `library(gradethis)`, or call `gradethis_setup()` in the setup chunk
#' of your tutorial. See `gradethis_demo()` to see an example learnr document
#' that uses `gradethis_exercise_checker()`.
#'
#' @examples
#' \dontrun{gradethis_demo()}
#' 
#' @param label Label for exercise chunk
#' @param solution_code Code provided within the "-solution" chunk for the
#'   exercise.
#' @param user_code R code submitted by the user
#' @param check_code Code provided within the "-check" (or "-code-check") chunk
#'   for the exercise.
#' @param envir_result The R environment after the execution of the chunk.
#' @param evaluate_result The return value from the `evaluate::evaluate`
#'   function.
#' @param envir_prep A copy of the R environment before the execution of the
#'   chunk.
#' @param last_value The last value from evaluating the user's exercise
#'   submission.
#' @param ... Extra arguments supplied by learnr
#'
#' @return Returns a feedback object suitable for \pkg{learnr} tutorials with
#'   the results of the exercise grading code.
#'
#' @seealso [gradethis_setup()], [grade_this()], [grade_this_code()]
#'
#' @export
gradethis_exercise_checker <- function(
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
  # Call this function in such a way that it can use other gradethis internals when called by learnr
  # (i.e., make tutorial_options(exercise.checker = gradethis::gradethis_exercise_checker) always work)
  utils::getFromNamespace("check_exercise", "gradethis")(
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

check_exercise <- function(
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
  
  if (length(user_code) == 0 || !any(nzchar(trimws(user_code)))) {
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

  ## setup environments for checking
  # envir for function call
  chunk_envir <- learnr::duplicate_env(envir_prep)
  # envir where checking is called (checking returns from here)
  checking_envir <- rlang::current_env()
  # envir to use for evaluating grade_this checking code
  check_obj_envir <- prepare_check_obj_envir(learnr_args, chunk_envir, checking_envir)

  # evaluate all checking code
  to_check_fn <- capture_errors(
    {
      parsed_check_code <- parse(text = check_code %||% "")
      capture_graded(
        {
          eval(parsed_check_code, envir = chunk_envir)
        },
        # if a `pass()`/`fail()` is used in the regular check chunk with no user submission context, it should be an error
        on_graded = function(grade, ignore) {
          fn_used <- if (isTRUE(grade$correct)) "`pass()`" else "`fail()`"

          # notify author of their mistake
          message(
            "A ", fn_used, " statement was executed without access to student feedback. (Prematurely graded)\n",
            "Remember to only call ", fn_used, " inside your checking function (ex: `grade_this({})`"
          )
          # return from main function (even though in a inner function! voodoo!)
          rlang::return_from(checking_envir, feedback_grading_problem())
        }
      )
    },
    # if an unhandled error occurs while checking...
    on_error = function(e, ignore) {
      # notify author of their mistake
      message("Error while checking `", check_label, "` chunk: ", e)
      # return from main function (even though in a inner function! voodoo!)
      rlang::return_from(checking_envir, feedback_grading_problem())
    }
  )

  tryCatch(
    parse(text = user_code %||% ""),
    error = function(e) {
      # Add the error object to the checking object
      check_obj_envir$.error <- e
      # Overwrite `to_check_fn` to validate the parse error function accepts `check_obj_envir`
      to_check_fn <<- getOption("exercise.parse.error", grade_parse_error)
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
    return(feedback_grading_problem())
  }

  # evaluate the function with the check envir passed in. (Passing an environment allows for `.solution` to be calculated on demand)
  # evaluation should handle errors themselves, but wrap in eval_gradethis to be sure
  graded_result <- eval_gradethis(
    to_check_fn(check_obj_envir)
  )

  # make sure the result is a pass or fail
  if (!is_graded(graded_result)) {
    message(
      "`", check_label, "` chunk did not mark an answer as correct or incorrect.",
      "Consider adding a `pass()` or `fail()` at the end of your `", check_label, "` code"
    )
    return(feedback_grading_problem())
  }

  # return result like normal
  feedback(
    graded_result,
    type = "auto"
  )
}


prepare_check_obj_envir <- function(learnr_args, envir_base, envir_caller = parent.frame()) {
  check_obj_envir <- new.env(parent = envir_base)
  
  # Copy over all learnr args into the checking environment
  for (name in names(learnr_args)) {
    learnr_arg <- learnr_args[[name]]
    name <- paste0(".", name)
    
    # Ensure that code objects are always a length-1 character string
    if (length(learnr_arg) > 1 && grepl("code", name) && is.character(learnr_arg)) {
      learnr_arg <- paste(learnr_arg, collapse = "\n")
    }
    
    check_obj_envir[[name]] <- learnr_arg
  }
  
  # Add gradethis specific check objects
  check_obj_envir[[".result"]] <- learnr_args[["last_value"]]
  check_obj_envir[[".user"]] <- learnr_args[["last_value"]]
  
  # Delayed evaluation of `.solution`
  solution_expr <- parse(text = learnr_args[["solution_code"]])
  delayedAssign(
    assign.env = check_obj_envir,
    x = ".solution",
    {
      if (length(solution_expr) == 0) {
        rlang::return_from(envir_caller, feedback(
          fail("No solution is provided for this exercise."),
          type = "info"
        ))
      } else {
        # solution code exists...
        # Using eval_tidy does not evaluate the expression. Using eval() instead
        eval(
          solution_expr,
          envir = learnr::duplicate_env(envir_base)
        )
      }
    }
  )
  check_obj_envir
}

grade_parse_error <- function(check_obj) {
  # check_obj contains everything in learnr_args plus...
  #   - .error (parse error condition)
  #   - .solution (evaluated .solution_code)
  #   - .result (.last_value from .user_code)
  #   - .user (.last_value from .user_code)
  #   
  # Code scaffolding in exercise code will cause parse errors, so first check
  # for blanks. We consider a blank to be 3+ "_" characters.
  n_blanks <- sum(vapply(
    gregexpr("_{3,}", check_obj$.user_code),
    function(x) sum(x > 0),
    integer(1)
  ))
  msg <- 
    if (n_blanks > 0) {
      paste0(
        "The exercise contains ", 
        if (n_blanks == 1L) {
          "1 blank"
        } else {
          paste(n_blanks, "blanks")
        },
        ". Please replace the `____` with valid R code."
      )
    } else {
      paste0(
        "It looks like this might not be valid R code:\n\n```r\n",
        conditionMessage(check_obj$.error),
        "\n```\n\nR cannot determine how to turn your text into ",
        "a complete command. You may have forgotten to fill in a blank, ",
        "to remove an underscore, to include a comma between arguments, ",
        "or to close an opening `\"`, `'`, `(`, or `{{` ",
        "with a matching `\"`, `'`, `)`, or `}}`. "
      )
    }
  fail(message = msg)
}
