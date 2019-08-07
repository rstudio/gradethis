context("Test Result")

expect_message <- function(x, message, correct = FALSE) {
  expect_s3_class(x, "grader_graded")
  expect_equal(x$correct, correct)
  expect_true(grepl(message, paste0(x$message, collapse = ""), fixed = TRUE))
}

test_that("Test formula type", {
  example_function <- function(x){
    return(x + 1)
  }

  expect_correct(
    test_result(
        pass_if(~ .result(3) == 4),
        pass_if(~ .result(10) == 11),
        grader_args = list(),
        learnr_args = list(last_value = example_function, envir_prep = new.env())
    )
  )

  expect_wrong(
    test_result(
        pass_if(~ .result(3) == 4),
        fail_if(~ .result(10) == 11),
        grader_args = list(),
        learnr_args = list(last_value = example_function, envir_prep = new.env())
    )
  )

  expect_wrong(
    test_result(
        pass_if(~ .result(100) == 1),
        grader_args = list(),
        learnr_args = list(last_value = example_function, envir_prep = new.env())
    )
  )
})
