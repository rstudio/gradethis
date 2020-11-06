context("Check grade_result_strict messages")

expect_message <- function(x, message, correct) {
  expect_s3_class(x, "gradethis_graded")
  expect_equal(x$correct, correct)
  expect_true(grepl(message, paste0(x$message, collapse = ""), fixed = TRUE))
}

test_that("Correct messages without random praise", {
  glue_correct_no_praise <- "{ .num_correct }/{ .num_total } correct!"
  example_function <- function(x){
    return(x + 1)
  }

  expect_grade_result_strict(
    last_value = example_function,
    is_correct = TRUE,
    msg = "2/2 correct!",
    glue_correct = glue_correct_no_praise,
    pass_if(~ .result(3) == 4),
    pass_if(~ .result(10) == 11)
  )

})

test_that("Incorrect mesages without random praise", {
  glue_incorrect_no_praise <- "{ .num_correct }/{ .num_total } correct!"
  example_function <- function(x){
    return(x + 1)
  }

  expect_grade_result_strict(
    last_value = example_function,
    is_correct = FALSE,
    msg = "1/2 correct!",
    pass_if(~ .result(3) == 4),
    fail_if(~ .result(10) == 11),
    glue_incorrect = glue_incorrect_no_praise
  )
})
