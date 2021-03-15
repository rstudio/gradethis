test_that("default error checker returns user error message", {
  feedback <- 
    expect_exercise_checker(
      "b",
      check_code = "set.seed(4242); gradethis_error_checker()",
      is_correct = FALSE,
      msg = I("object 'b' not found")
    )
  
  expect_match(feedback$message, {set.seed(4242); random_encouragement()}, fixed = TRUE)
})

test_that("default error checker returns code feedback", {
  feedback <- 
    expect_exercise_checker(
      "b",
      solution_code = "a",
      check_code = "set.seed(4242); gradethis_error_checker()",
      is_correct = FALSE,
      msg = I("I expected")
    )
  
  expect_match(feedback$message, {set.seed(4242); random_encouragement()}, fixed = TRUE)
})

