context("Check pass_if")

test_that("Pass if", {

  expect_true(pass_if(
    ~ .result == 5,
    message = "message",
    user_answer = 5, grader_args = list(),
    learnr_args = list(envir_prep = new.env()))
  )

})

context("Check fail_if")
