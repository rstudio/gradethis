test_that("allow_partial_matching works", {
  
  default <- grade_code(
    grader_args = list(
      user_quo = expression(read_csv(file = "foo.csv", col_nam = TRUE,quoted_ = TRUE)), 
      solution_quo = expression(read_csv(file = "foo.csv", col_names = TRUE,quoted_na = TRUE))
    )
  )
  pmatch_FALSE <- grade_code(allow_partial_matching = FALSE,
             grader_args = list(
               user_quo = expression(read_csv(file = "foo.csv", col_nam = TRUE,quoted_ = TRUE)), 
               solution_quo = expression(read_csv(file = "foo.csv", col_names = TRUE,quoted_na = TRUE))
             )
  )
  pmatch_TRUE <- grade_code(allow_partial_matching = TRUE,
             grader_args = list(
               user_quo = expression(read_csv(file = "foo.csv", col_nam = TRUE,quoted_ = TRUE)), 
               solution_quo = expression(read_csv(file = "foo.csv", col_names = TRUE,quoted_na = TRUE))
             )
  )
  
  expect_true(default$correct)
  expect_true(pmatch_TRUE$correct)
  expect_false(pmatch_FALSE$correct)
  expect_match(object = pmatch_FALSE$message,regexp = "col_names = TRUE")
  expect_match(object = pmatch_FALSE$message,regexp = "quoted_na = TRUE")
  
  
})
