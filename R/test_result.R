# TODO document
#' @export
test_result <- function(
  tests,
  correct = "{num_correct}/{num_total} correct! {random_praise()}",
  incorrect = "{num_correct}/{num_total} correct. Fix this first: '{errors[1]}'. {random_encourage()}",
  empty_msg = "I did not notice a result. Does your code return one?",
  solution = NULL, user = NULL, # provided by `grade_learnr`
  envir_result = globalenv(),
  ... # ignored / extra params
) {
  chkm8_class(tests, "grader_tests")
  chkm8_single_character(correct)
  chkm8_single_character(incorrect)
  chkm8_single_character(empty_msg)
  checkmate::assert_environment(envir_result)

  user_answer <- get_user_code(user)
  if (is.null(user_answer)) {
    return(result(user_answer, message = empty_msg, correct = FALSE))
  }

  results <- lapply(tests$fns, function(test_fn) {
    is_correct <- TRUE
    tryCatch(
      {
        test_fn(user_answer)
        result(
          NULL,
          message = NULL,
          correct = TRUE
        )
      },
      error = function(e) {
        result(
          NULL,
          message = e,
          correct = FALSE
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

  return(result(
    x = user_answer,
    message = message,
    correct = is_correct
  ))
}


# TODO document
#' @export
tests <- function(...) {
  fns <- list(...)
  lapply(fns, checkmate::assert_function)
  structure(
    class = "grader_tests",
    list(
      fns = list(...)
    )
  )
}

# TODO document
#' @export
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
