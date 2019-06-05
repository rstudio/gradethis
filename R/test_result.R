#' Test the result of exercise code
#'
#' \code{test_result()} executes tests against the final result of the user code.
#' If a test throws an error, the test fails and the submitted answer will be marked incorrect.
#'
#' @param tests A \code{\link{tests}} object that contains all \code{\link{test}} functions to check the user's result.
#' @param correct A character string to display if all tests pass.
#'   This character string will be run through \code{glue::\link[glue]{glue_data}} with
#' \itemize{
#'   \item \code{num_correct}: Number of correct tests. (Equals \code{num_total})
#'   \item \code{num_total}: Number of tests
#'   \item \code{errors}: Vector of errors found. (\code{NULL})
#' }
#' @param incorrect A character string to display if at least one test fails.
#'   This character string will be run through \code{glue::\link[glue]{glue_data}} with
#' \itemize{
#'   \item \code{num_correct}: Number of correct tests
#'   \item \code{num_total}: Number of tests
#'   \item \code{errors}: Vector of errors found
#' }
#' @param empty_msg A character string to display as a message if the user code is NULL.
#' @param ... ignored
#' @param user (Optional) student code to check against the \code{results} surrounded
#'   by \code{quote()}, \code{rlang::quo()}, or provided as a character string.
#'
#' @return a \code{grader_result} structure from \code{\link{result}} containing a formatted \code{correct} or \code{incorrect} message.
#' @seealso \code{tests}, \code{test}
#' @export
#' @examples
#' \dontrun{grading_demo()}
test_result <- function(
  ...,
  correct = "{num_correct}/{num_total} correct! {random_praise()}",
  incorrect = paste0(
    "{num_correct}/{num_total} correct. ",
    "Fix this first: '{errors[1]}'. ",
    "{random_encourage()}"
  ),
  empty_msg = "I did not notice a result. Does your code return one?",
  grader_args = list(), # provided by `grade_learnr`
  learnr_args = list() # provided by `grade_learnr`
) {
  tests <- tests(...)
  chkm8_class(tests, "grader_tests")
  chkm8_single_character(correct)
  chkm8_single_character(incorrect)
  chkm8_single_character(empty_msg)

  user_answer <- get_user_code(grader_args$user_quo)
  if (is.null(user_answer)) {
    return(graded(correct = FALSE, message = empty_msg))
  }

  results <- lapply(tests$fns, function(test_fn) {
    tryCatch(
      {
        test_fn(user_answer)
        graded(
          correct = TRUE,
          message = NULL
        )
      },
      error = function(e) {
        graded(
          correct = FALSE,
          message = as.character(e)
        )
      }
    )
  })

  is_corrects <- vapply(results, `[[`, logical(1), "correct")
  is_correct <- all(is_corrects)

  message <- glue::glue_data(
    list(
      is_correct = is_correct,
      num_correct = sum(is_corrects),
      num_total = length(results),
      errors = unlist(lapply(results, function(resu) {
        if (!resu$correct) resu$message else NULL
      }))
    ),
    {if (is_correct) correct else incorrect}
  )

  return(graded(
    correct = is_correct,
    message = message
  ))
}

# TODO do not use anymore in favor of `...` arg
#' Tests to check
#'
#' Collect a set of test to execute against a user's result value
#' @param ... a set of functions that will accept the evaluated user solution. If the test fails, it should throw an error with the message to display.
#' @noRd
#' @rdname test
#' @examples
#'
#' tests(
#'   function(your_answer) {
#'     checkmate::expect_function(your_answer, args = c("x"))
#'   },
#'   test(
#'     # use a custom error message
#'     "Make sure your function returns a number!",
#'     function(your_answer) {
#'       checkmate::expect_number(your_answer(1))
#'     }
#'   ),
#'   function(your_answer) {
#'     testthat::expect_equal(your_answer(0), NaN)
#'   },
#'   function(your_answer) {
#'     testthat::expect_equal(your_answer(1:10), sqrt(log(1:10)))
#'   }
#' )
#'
#' \dontrun{grading_demo()}
tests <- function(...) {
  fns <- list(...)
  lapply(fns, function(fn) {
    checkmate::assert_function(fn)
    if (length(formals(fn)) == 0) {
      stop("The function must be able to accept the user submission")
    }
  })
  structure(
    class = "grader_tests",
    list(
      fns = list(...)
    )
  )
}
#' TODO document with 'tests' documentation
#' @export
#' @rdname test
#' @param message Message to report back if the test throws an error.
#' @param fn function to execute against the user solution.  If the test fails, it should throw an error to display the \code{message} provided.
test <- function(message, fn) {
  function(x) {
    tryCatch(
      {
        fn(x)
      },
      error = function(e) {
        stop(message, call. = FALSE)
      }
    )
  }
}
