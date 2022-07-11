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
  with_options(
    list(gradethis.fail.hint = TRUE),
    expect_exercise_checker(
      "definitely_not_right",
      solution_code = "a",
      check_code = "gradethis_error_checker()",
      is_correct = FALSE,
      msg = I("I expected")
    )
  )
})

test_that("default error checker hides code feedback if `!gradethis.fail.hint`", {
  with_options(
    list(gradethis.fail.hint = FALSE),
    expect_exercise_checker(
      "definitely_not_right",
      solution_code = "a",
      check_code = "gradethis_error_checker()",
      is_correct = FALSE,
      msg = I("An error occurred")
    )
  )
})
