test_that("default error checker returns user error message", {
  feedback <- 
    expect_exercise_checker(
      "b",
      check_code = "gradethis_error_checker()",
      is_correct = FALSE,
      msg = I("object 'b' not found")
    )
})

test_that("default error checker returns code feedback", {
  feedback <- 
    expect_exercise_checker(
      "b",
      solution_code = "a",
      check_code = "gradethis_error_checker()",
      is_correct = FALSE,
      msg = I("I expected")
    )
})

