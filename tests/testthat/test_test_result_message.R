context("Check grade_conditions messages")

expect_message <- function(x, message, correct) {
  expect_s3_class(x, "grader_graded")
  expect_equal(x$correct, correct)
  expect_true(grepl(message, paste0(x$message, collapse = ""), fixed = TRUE))
}

test_that("Correct messages without random praise", {
  glue_correct_no_praise <- "{ .num_correct }/{ .num_total } correct!"
  example_function <- function(x){
    return(x + 1)
  }

  expect_message(
    grade_conditions(
        pass_if(~ .result(3) == 4),
        pass_if(~ .result(10) == 11),
        grader_args = list(),
        learnr_args = list(last_value = example_function, envir_prep = new.env()),
        glue_correct = glue_correct_no_praise
    ),
    message = "2/2 correct!",
    correct = TRUE
  )
})

test_that("Incorrect mesages without random praise", {
  glue_incorrect_no_praise <- "{ .num_correct }/{ .num_total } correct!"
  example_function <- function(x){
    return(x + 1)
  }

  expect_message(
    grade_conditions(
      pass_if(~ .result(3) == 4),
      fail_if(~ .result(10) == 11),
      grader_args = list(),
      learnr_args = list(last_value = example_function, envir_prep = new.env()),
      glue_incorrect = glue_incorrect_no_praise
    ),
    message = "1/2 correct!",
    correct = FALSE
  )

})
