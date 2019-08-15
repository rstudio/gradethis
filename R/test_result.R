#' Test the result of exercise code.
#'
#' Executes tests against the final result of the user code. If
#' a test throws an error, the test fails and the submitted answer will be
#' marked incorrect.
#'
#' @inheritParams check_code
#'
#' @param correct A character string to display if all tests pass. This
#'   character string will be run through `[glue::glue_data]` with:
#'
#'   * `num_correct`: Number of correct tests. (Equals `num_total`)
#'
#'   * `num_total`: Number of tests
#'
#'   * `errors`: Vector of errors found. (`NULL`)
#'
#' @param incorrect A character string to display if at least one test fails.
#'   This character string will be run through `][glue::glue_data]` with:
#'
#'   * `num_correct`: Number of correct tests
#'
#'   * `num_total`: Number of tests
#'
#'   * `errors`: Vector of errors found
#'
#' @param ... ignored
#'
#'
#' @return a `grader_graded` structure from [graded()] containing a formatted
#'   `correct` or `incorrect` message.
#' @seealso `test`
#' @export
#' @examples
#' \dontrun{grading_demo()}
test_result <- function(
  ...,
  correct = "{num_correct}/{num_total} correct! {random_praise()}",
  incorrect = "",
  grader_args = list(),
  learnr_args = list()
) {
  tests <- grader_tests(...)
  chkm8_class(tests, "grader_tests")
  chkm8_single_character(correct)
  chkm8_single_character(incorrect)

  user_answer <- get_user_code(grader_args$user_quo)

  results <- lapply(tests$fns, function(test_fn) {
    tryCatch(
      { # nolint
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
    {if (is_correct) correct else incorrect} # nolint
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
#' @param ... a set of functions that will accept the evaluated user solution.
#'   If the test fails, it should throw an error with the message to display.
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
grader_tests <- function(...) {
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
