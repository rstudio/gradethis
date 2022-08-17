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
#' \dontrun{
#' gradethis_demo()
#' }
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
#' @param stage The current stage of exercise checking.
#' @param ... Extra arguments supplied by learnr
#' @param solution_eval_fn A function taking solution `code` and an `envir` (an
#'   environment equivalent to `envir_prep`) and that will return the value of
#'   the evaluated `code`. This callback function allows grading authors to
#'   write custom solution evaluation functions for non-R exercise engines. The
#'   result of the evaluated code should be an R object that will be accessible
#'   to the grading code in [.solution] or [.solution_all].
#'
#'   You may also provide a named list of solution evaluation functions to the
#'   `gradethis.exercise_checker.solution_eval_fn` global option. The names of
#'   the list should match the exercise engine for which the function should
#'   be applied.
#'
#'   For example, for a hypothetical exercise engine `echo` that simply echoes
#'   the user's code, you could provide a `solution_eval_fn` that also just
#'   echoes the solution code:
#'
#'   ```
#'   options(
#'     gradethis.exercise_checker.solution_eval_fn = list(
#'       echo = function(code, envir) {
#'         code
#'       }
#'     )
#'   )
#'   ```
#'
#'  Solution evaluation functions should determine if the solution code is
#'  missing and if so throw an error with class `error_missing_solution` (see
#'  [rlang::abort()] for help throwing this error).
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
  stage = NULL,
  ...,
  solution_eval_fn = NULL
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
    stage = stage,
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
  stage = NULL,
  ...,
  solution_eval_fn = NULL
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
    stage = stage,
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
  # Errors in setup of exercise checking return from here
  check_exercise_env <- rlang::current_env()

  # Create the check environment used by grade_this() where the parent of
  # check_env is a clone of `envir_prep`. We use capture_errors/graded to throw
  # an internal grading problem if anything goes wrong.
  check_env <- capture_errors(
    capture_graded(
      prepare_check_env(learnr_args, solution_eval_fn = solution_eval_fn)
    ),
    on_error = function(err, ...) {
      grade_grading_problem("Could not prepare checking environment for gradethis checking code.", error = err)
    }
  )

  if (is_graded(check_env)) {
    # An error occurred while trying to prepare the check environment
    return(feedback(check_env, "error"))
  }

  # evaluate all checking code
  to_check_fn <- capture_errors(
    {
      parsed_check_code <- parse(text = check_code %||% "")
      capture_graded(
        {
          eval(parsed_check_code, envir = rlang::env_parent(check_env))
        },
        # if a `pass()`/`fail()` is used in the regular check chunk with no user submission context, it should be an error
        on_graded = function(grade, ignore) {
          fn_used <- if (isTRUE(grade$correct)) "`pass()`" else "`fail()`"

          # notify author of their mistake
          err_premature_grading <- list(
            message = sprintf(
              paste(
                "A %s statement was executed without access to student feedback (prematurely graded).",
                "Remember to only call %s inside your checking function, e.g. `grade_this({})`."
              ),
              fn_used, fn_used
            ),
            call = check_code,
            label = check_label
          )
          message(err_premature_grading$message)
          # return from main function (even though in a inner function! voodoo!)
          rlang::return_from(
            check_exercise_env,
            feedback_grading_problem(error = err_premature_grading)
          )
        }
      )
    },
    # if an unhandled error occurs while checking...
    on_error = function(e, ignore) {
      # notify author of their mistake
      message("Error while checking `", check_label, "` chunk: ", e)
      e$label <- check_label
      e$call <- check_code
      # return from main function (even though in a inner function! voodoo!)
      rlang::return_from(check_exercise_env,  feedback_grading_problem(error = e))
    }
  )

  # Skip parse checking if it was already done in {learnr}
  if (!learnr_includes_parse_check(stage) && identical(learnr_args$engine %||% "r", "r")) {
    tryCatch(
      parse(text = user_code %||% ""),
      error = function(e) {
        # Add the error object to the checking object
        check_env$.error <- e
        # Overwrite `to_check_fn` to validate the parse error function accepts `check_obj_envir`
        to_check_fn <<- getOption("exercise.parse.error", grade_parse_error)
      }
    )
  }

  if (
    !(
      # make sure the returned value from check chunk evaluation is a function
      checkmate::test_function(to_check_fn) &&
        # ...that accepts at least 1 argument
        checkmate::test_number(length(formals(to_check_fn)), lower = 1)
    )
  ) {
    # notify author of their mistake
    err_not_a_function <- list(
      message = paste0(
        "`", check_label, "` chunk did not return a function (such as `grade_this`) ",
        "that accepts 1 argument containing the checking object"
      ),
      call = check_code,
      label = check_label
    )
    message(err_not_a_function$message, "\nObject returned:\n", err_not_a_function$call)
    return(feedback_grading_problem(error = err_not_a_function))
  }

  # evaluate the function with the check envir passed in. (Passing an environment allows for `.solution` to be calculated on demand)
  # evaluation should handle errors themselves, but wrap in eval_gradethis to be sure
  graded_result <- eval_gradethis(
    to_check_fn(check_env)
  )

  # make sure the result is a pass or fail
  if (!is_graded(graded_result)) {
    if (!is.null(stage) && !identical(tolower(stage), "check")) {
      # check chunks require a final answer, others might defer to later stages
      return()
    }

    err_result_not_graded <- list(
      message = paste0(
        "`", check_label, "` chunk did not mark an answer as correct or incorrect.",
        "Consider adding a `pass()` or `fail()` at the end of your `", check_label, "` code"
      ),
      call = user_code,
      label = check_label
    )
    message(err_result_not_graded$message)
    return(feedback_grading_problem(error = err_result_not_graded))
  }

  # return result like normal
  feedback(
    graded_result,
    type = "auto"
  )
}


prepare_check_env <- function(
  learnr_args,
  envir_caller = rlang::caller_env(),
  solution_eval_fn = NULL
) {
  # The check_env starts from a copy of envir_prep which is the result of
  # evaluating all of exercise setup code, duplicated to avoid the possibility
  # of the checking code changing the prep environment
  envir_base <- learnr::duplicate_env(learnr_args[["envir_prep"]])
  check_env <- new.env(parent = envir_base)

  force(envir_caller)

  # Copy over all learnr args into the checking environment
  for (name in names(learnr_args)) {
    learnr_arg <- learnr_args[[name]]
    name <- paste0(".", name)

    # Ensure that code objects are always a length-1 character string
    if (length(learnr_arg) > 1 && grepl("code", name) && is.character(learnr_arg)) {
      learnr_arg <- paste(learnr_arg, collapse = "\n")
    }

    check_env[[name]] <- learnr_arg
  }

  # Add gradethis specific check objects
  check_env[[".result"]] <- learnr_args[["last_value"]]
  check_env[[".user"]] <- learnr_args[["last_value"]]

  # Add full solution code options
  solutions <- solutions_prepare(learnr_args[["solution_code"]])
  check_env[[".solution_code_all"]] <- solutions

  if (inherits(solutions, "gradethis_solutions")) {
    # use last solution for .solution_code if we have multiple solutions
    check_env[[".solution_code"]] <- solutions[[length(solutions)]]
  }

  # Delayed evaluation of `.solution` and `.solution_all`
  if (is.null(solution_eval_fn)) {
    solution_eval_fn <- solution_eval_fn_get(
      engine = learnr_args[["engine"]],
      learnr_args[["label"]]
    )
  }

  solution_eval_delayed(
    code = check_env[[".solution_code"]] %||% "",
    name = ".solution",
    envir_assign = check_env,
    envir_eval = envir_base,
    envir_caller = envir_caller,
    exercise_label = learnr_args[["label"]],
    eval_fn = solution_eval_fn
  )

  check_env[[".solution_all"]] <-
    solution_eval_all_delayed(
      solutions,
      envir_base,
      eval_fn = solution_eval_fn
    )

  check_env
}

solution_eval_all_delayed <- function(
  solution_code_all = NULL,
  envir_base = parent.frame(),
  eval_fn = NULL
) {
  if (is.null(solution_code_all) || length(solution_code_all) == 0) {
    return(NULL)
  }

  if (!inherits(solution_code_all, "gradethis_solutions")) {
    solution_code_all <- solutions_prepare(solution_code_all)
  }

  solutions_env <- new.env()
  class(solutions_env) <- "gradethis_solutions_env"

  names_original <- names(solution_code_all) %||% "solution"
  names(names_original) <- make.unique(names_original, sep = "_")
  names(solution_code_all) <- names(names_original)
  assign(".solution_labels", names_original, solutions_env)

  purrr::iwalk(
    solution_code_all,
    solution_eval_delayed,
    envir_eval = envir_base,
    envir_assign = solutions_env,
    eval_fn = eval_fn
  )

  solutions_env
}

solution_not_provided_grade <- function(label = NULL) {
  grade_grading_problem(
    message = "No solution is provided for this exercise.",
    type = "warning",
    error = list(
      message = "No solution provided for this exercise",
      label = label
    )
  )
}

solution_eval_delayed <- function(
  code,
  name = ".solution",
  envir_assign,
  envir_eval,
  eval_fn = NULL,
  exercise_label = NULL,
  envir_caller = rlang::caller_env()
) {
  delayedAssign(
    assign.env = envir_assign,
    x = name,
    {
      tryCatch(
        eval_fn(code, envir_eval),
        error_missing_solution = function(err) {
          grade_no_solution <- solution_not_provided_grade(exercise_label)

          if (!is.null(envir_caller)) {
            # inside gradethis_exercise_checker or another process,
            # return feedback from there
            rlang::return_from(envir_caller, feedback(grade_no_solution))
          }

          # otherwise (e.g. mocking) just return the solution problem grade
          grade_no_solution
        }
      )
    }
  )
}

solution_eval_fn_get <- function(engine, label = NULL) {
  default_fns <- list(
    r = solution_eval_r,
    sql = solution_eval_sql
  )

  user_defined <- getOption("gradethis.exercise_checker.solution_eval_fn", list())
  names(user_defined) <- tolower(names(user_defined))
  fns_list <- utils::modifyList(default_fns, user_defined)

  if (!tolower(engine) %in% names(fns_list)) {
    return(solution_eval_fn_not_defined(label, engine))
  }

  fns_list[[tolower(engine)]]
}

solution_eval_r <- function(code, envir) {
  expr <- parse(text = code)
  if (length(expr) == 0) {
    rlang::abort(class = "error_missing_solution")
  }

  eval(expr, envir = envir)
}

solution_eval_sql <- function(code, envir) {
  rlang::check_installed("DBI")

  # Find the DB connection in the `envir` objects
  objs <- lapply(ls(envir), get, envir = envir)
  is_db_con <- vapply(objs, inherits, logical(1), "DBIConnection")
  con <- objs[is_db_con][[1]]

  # Execute the query
  DBI::dbGetQuery(con, code)
}

solution_eval_fn_not_defined <- function(label, engine) {
  engine <- knitr_engine_caption(engine)
  msg <- glue::glue("{gradethis_settings$grading_problem.message()} Solution results are not available for {engine} code.")

  function(...) {
    grade_grading_problem(
      message = msg,
      type = "warning",
      error = list(message = msg, label = label)
    )
  }
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
  fail(message = msg, error = list(message = check_obj$.error$message, call = check_obj$.user_code))
}

learnr_includes_parse_check <- function(stage) {
  if (is.null(stage)) {
    return(FALSE)
  }
  utils::packageVersion("learnr") >= package_version("0.10.1.9017")
}
