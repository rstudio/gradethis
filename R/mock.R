#' Mock a user submission to an exercise
#'
#' This function helps you test your [grade_this()] and [grade_this_code()]
#' logic by helping you quickly create the environment that these functions
#' expect when used to grade a user submission to an exercise in a \pkg{learnr}
#' tutorial.
#'
#' @param .user_code A single string or expression in braces representing the
#'   user submission to this exercise.
#' @param .solution_code An optional single string or expression in braces
#'   representing the solution code to this exercise.
#' @param .label The label of the mock exercise, defaults to `"mock"`.
#' @param .stage The stage of the exercise evaluation, defaults to `"check"`.
#'   \pkg{learnr} stages are `"code_check"`, `"check"` or `"error_check"`. When
#'   gradethis is used outside of learnr, this variable is typically `NULL`.
#' @param .engine The engine of the mock exercise. If the engine is not `"r"`,
#'   then `.result` must be provided explicitly since `mock_this_exercise()`
#'   cannot evaluate the `.user_code`.
#' @param .result The result of the evaluation of the `.user_code`. If the
#'   `.engine` is `"r"`, the result will be prepared automatically by evaluating
#'   the user code.
#' @param setup_global An optional single string or expression in braces
#'   representing the global `setup` chunk code.
#' @param setup_exercise An optional single string or expression in braces
#'   representing the code in the exercise's setup chunk(s).
#' @param ... Ignored
#'
#' @return Returns the checking environment that is expected by [grade_this()]
#'   and [grade_this_code()]. Both of these functions themselves return a
#'   function that gets called on the checking environment. In other words, the
#'   object returned by this function can be passed to the function returned
#'   from either [grade_this()] or [grade_this_code()] to test the grading
#'   logic used in either.
#'
#' @examples
#' # First we'll create a grading function with grade_this(). The user's code
#' # should return the value 42, and we have some specific messages if they're
#' # close but miss this target. Otherwise, we'll fall back to the default fail
#' # message, which will include code feedback.
#' this_grader <-
#'   grade_this({
#'     pass_if_equal(42, "Great Work!")
#'     fail_if_equal(41, "You were so close!")
#'     fail_if_equal(43, "Oops, just missed!")
#'     fail()
#'   })
#'
#' # Our first mock submission is almost right...
#' this_grader(mock_this_exercise(.user_code = 41, .solution_code = 42))
#'
#' # Our second mock submission is a little too high...
#' this_grader(mock_this_exercise(.user_code = 43, .solution_code = 42))
#'
#' # A third submission takes an unusual path, but arrives at the right answer.
#' # Notice that you can use braces around an expression.
#' this_grader(
#'   mock_this_exercise(
#'     .user_code = {
#'       x <- 31
#'       y <- 11
#'       x + y
#'     },
#'     .solution_code = 42
#'   )
#' )
#'
#' # Our final submission changes the prompt slightly. Suppose we have provided
#' # an `x` object in our global setup with a value of 31. We also have a `y`
#' # object that we create for the user in the exercise setup chunk. We then ask
#' # the student to add `x` and `y`. What happens if the student subtracts
#' # instead? That's what this mock submission tests:
#' this_grader(
#'   mock_this_exercise(
#'     .user_code = x - y,
#'     .solution_code = x + y,
#'     setup_global = x <- 31,
#'     setup_exercise = y <- 11
#'   )
#' )
#' @export
mock_this_exercise <- function(
  .user_code,
  .solution_code = NULL,
  ...,
  .label = "mock",
  .engine = "r",
  .stage = "check",
  .result = rlang::missing_arg(),
  .error = rlang::missing_arg(),
  setup_global = NULL,
  setup_exercise = NULL
) {
  .engine <- tolower(.engine)
  is_r_code <- identical(.engine, "r")

  env_global <- rlang::env(globalenv())

  .user_code <- rlang::enexpr(.user_code)
  .solution_code <- rlang::enexpr(.solution_code)
  setup_global <- rlang::enexpr(setup_global)
  setup_exercise <- rlang::enexpr(setup_exercise)

  eval_code(setup_global, env_global)

  env_prep <- rlang::env(env_global)
  eval_code(setup_exercise, env_prep)

  env_result <- rlang::env_clone(env_prep, env_global)
  if (rlang::is_missing(.result)) {
    if (!is_r_code) {
      rlang::abort(glue::glue(
        "Must provide `.result` for a mock exercise with `.engine = \"{.engine}\"`"
      ))
    }
    tryCatch({
      .result <- eval_code(.user_code, env_result)
    },
    error = function(e) .error <<- e
    )
  }

  learnr_args <- list(
    label = .label,
    solution_code = expr_text(.solution_code, is_r_code),
    user_code = expr_text(.user_code, is_r_code),
    envir_result = env_result,
    # evaluate_result = evaluate_result,
    envir_prep = env_prep,
    last_value = .result,
    stage = .stage,
    engine = .engine,
    ...
  )

  if (!rlang::is_missing(.error)) {
    learnr_args$error <- .error
    learnr_args$last_value <- .error
  }

  prepare_check_env(learnr_args, envir_caller = NULL)
}

eval_code <- function(x, env) {
  if (is.null(x)) return()
  if (rlang::is_string(x)) {
    x <- rlang::parse_exprs(x)
    .res <- lapply(x, rlang::eval_bare, env = env)
    # last value of result
    .res[[length(.res)]]
  } else {
    rlang::eval_bare(x, env)
  }
}

expr_text <- function(expr, is_r_code = TRUE) {
  if (rlang::is_null(expr)) {
    return()
  }
  if (rlang::is_string(expr)) {
    return(expr)
  }
  if (!isTRUE(is_r_code)) {
    rlang::abort("Bare expressions are only valid for exercises with R code engines")
  }
  if (length(expr) > 1 && identical(expr[[1]], rlang::sym("{"))) {
    # unwrap one level of braces
    x <- vapply(as.list(expr[-1]), FUN.VALUE = character(1), rlang::expr_text)
    paste(x, collapse = "\n")
  } else {
    rlang::expr_text(expr)
  }
}
