test_that("allow_partial_matching works", {
  
 user_quo <- as.expression(quote(purrr::insistently(mean,quie = TRUE,rat = rate_backoff())))
 solution_quo <- as.expression(quote(purrr::insistently(mean,quiet = TRUE,rate = rate_backoff())))
  
  
  default <- grade_code(
    grader_args = list(
      user_quo = user_quo, 
      solution_quo = solution_quo
    )
  )
  pmatch_FALSE <- grade_code(allow_partial_matching = FALSE,
             grader_args = list(
               user_quo = user_quo, 
               solution_quo = solution_quo
             )
  )
  pmatch_TRUE <- grade_code(allow_partial_matching = TRUE,
             grader_args = list(
               user_quo = user_quo, 
               solution_quo = solution_quo
             )
  )
 
  
  
  expect_true(default$correct)
  expect_true(pmatch_TRUE$correct)
  expect_false(pmatch_FALSE$correct)
  expect_match(object = pmatch_FALSE$message,regexp = "quie = TRUE")
  expect_match(object = pmatch_FALSE$message,regexp = "quiet = TRUE")
  expect_match(object = pmatch_FALSE$message,regexp = "rat = rate_backoff")
  expect_match(object = pmatch_FALSE$message,regexp = "rate = rate_backoff")
  
  
})

# test_that("allow_partial_matching works", {
#   
#   default <- grade_code(
#     grader_args = list(
#       user_quo = expression(readr::read_csv(file = "foo.csv", col_nam = TRUE,quoted_ = TRUE)), 
#       solution_quo = expression(readr::read_csv(file = "foo.csv", col_names = TRUE,quoted_na = TRUE))
#     )
#   )
#   pmatch_FALSE <- grade_code(allow_partial_matching = FALSE,
#              grader_args = list(
#                user_quo = expression(readr::read_csv(file = "foo.csv", col_nam = TRUE,quoted_ = TRUE)), 
#                solution_quo = expression(readr::read_csv(file = "foo.csv", col_names = TRUE,quoted_na = TRUE))
#              )
#   )
#   pmatch_TRUE <- grade_code(allow_partial_matching = TRUE,
#              grader_args = list(
#                user_quo = expression(readr::read_csv(file = "foo.csv", col_nam = TRUE,quoted_ = TRUE)), 
#                solution_quo = expression(readr::read_csv(file = "foo.csv", col_names = TRUE,quoted_na = TRUE))
#              )
#   )
#   
#   expect_true(default$correct)
#   expect_true(pmatch_TRUE$correct)
#   expect_false(pmatch_FALSE$correct)
#   expect_match(object = pmatch_FALSE$message,regexp = "col_names = TRUE")
#   expect_match(object = pmatch_FALSE$message,regexp = "quoted_na = TRUE")
#   
#   
# })