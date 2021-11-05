test_that("default error checker returns user error message", {
  feedback <-
    expect_exercise_checker(
      "definitely_not_right",
      check_code = "gradethis_error_checker()",
      is_correct = FALSE,
      msg = I("object 'definitely_not_right' not found")
    )
})

test_that("default error checker returns code feedback", {
  feedback <-
    expect_exercise_checker(
      "definitely_not_right",
      solution_code = "a",
      check_code = "gradethis_error_checker()",
      is_correct = FALSE,
      msg = I("I expected")
    )
})
