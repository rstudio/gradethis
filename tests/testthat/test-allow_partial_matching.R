test_that("allow_partial_matching works 2 errors", {
  
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



test_that("allow_partial_matching works 1 error bool", {
  
  user_quo <- as.expression(quote(purrr::insistently(mean,quie = TRUE)))
  solution_quo <- as.expression(quote(purrr::insistently(mean,quiet = TRUE)))
  
  
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
  
  
})



test_that("allow_partial_matching works 1 error fun", {
  
  user_quo <- as.expression(quote(purrr::insistently(mean,rat = rate_backoff())))
  solution_quo <- as.expression(quote(purrr::insistently(mean,rate = rate_backoff())))
  
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
  expect_match(object = pmatch_FALSE$message,regexp = "rat = rate_backoff")
  expect_match(object = pmatch_FALSE$message,regexp = "rate = rate_backoff")
  
  
})




test_that("allow_partial_matching works 1 error chr", {
  
  ff <- function(p1 = "yes"){
    print(p1)
  }
  
  user_quo <- as.expression(quote(ff(p="no")))
  solution_quo <- as.expression(quote(ff(p1="no")))
  
  pmatch_FALSE <- grade_code(allow_partial_matching = FALSE,
                             grader_args = list(
                               user_quo = user_quo, 
                               solution_quo = solution_quo
                             )
  )

  
  
  
  expect_false(pmatch_FALSE$correct)
  expect_match(object = pmatch_FALSE$message,regexp = "p = \"no\"")
  expect_match(object = pmatch_FALSE$message,regexp = "p1 = \"no\"")
  
  
})

test_that("allow_partial_matching works errors multi arg type", {
  
  ff <- function(chr = "yes", fun = ls, call =ls(), bool = TRUE){
    print("youpi")
  }
  
  solution_quo <- as.expression(quote(ff(chr = "yes", fun = ls, call =ls(), bool = TRUE)))
  user_quo <- as.expression(quote(ff(ch = "yes", fu = ls, cal =ls(), boo = TRUE)))
  
  
  pmatch_FALSE <- grade_code(allow_partial_matching = FALSE,
                             grader_args = list(
                               user_quo = user_quo, 
                               solution_quo = solution_quo
                             )
  )

  
  
  
  expect_false(pmatch_FALSE$correct)
  expect_match(object = pmatch_FALSE$message,regexp = "ch = \"yes\"")
  expect_match(object = pmatch_FALSE$message,regexp = "chr = \"yes\"")
  expect_match(object = pmatch_FALSE$message,regexp = "boo = TRUE")
  expect_match(object = pmatch_FALSE$message,regexp = "bool = TRUE")
  expect_match(object = pmatch_FALSE$message,regexp = "cal = ls()")
  expect_match(object = pmatch_FALSE$message,regexp = "call = ls()")
  expect_match(object = pmatch_FALSE$message,regexp = "fun = ls")
  expect_match(object = pmatch_FALSE$message,regexp = "fu = ls")
  
  
})

test_that("allow_partial_matching works with multiple matches", {
  
  ff <-  function(ab, abc, abcd) { 1 }
  
  
  solution_quo <- as.expression(quote(ff(ab = 1)))
  user_quo <- as.expression(quote(ff(ab = 1)))
  pmatch_FALSE <- grade_code(allow_partial_matching = FALSE,
                             grader_args = list(
                               user_quo = user_quo, 
                               solution_quo = solution_quo
                             ))
  
  expect_true(pmatch_FALSE$correct)
  
  
  solution_quo <- as.expression(quote(ff(a = 1)))
  user_quo <- as.expression(quote(ff(a = 1)))
  pmatch_FALSE <- grade_code(allow_partial_matching = FALSE,
                             grader_args = list(
                               user_quo = user_quo, 
                               solution_quo = solution_quo
                             ))
  
  expect_true(pmatch_FALSE$correct)
  
  
  
  solution_quo <- as.expression(quote(ff(abc=3)))
  user_quo <- as.expression(quote(ff(ab = 1,abc = 1)))
  
  set.seed(1)
  pmatch_FALSE <- grade_code(allow_partial_matching = FALSE,
                             grader_args = list(
                               user_quo = user_quo, 
                               solution_quo = solution_quo
                             ))
  set.seed(1)
  pmatch_TRUE <- grade_code(allow_partial_matching = TRUE,
                             grader_args = list(
                               user_quo = user_quo, 
                               solution_quo = solution_quo
                             ))

  
  expect_false(pmatch_FALSE$correct)
  expect_false(pmatch_TRUE$correct)
  expect_equal(pmatch_FALSE,pmatch_TRUE)
  
  solution_quo <- as.expression(quote(ff(ab = 1,abc = 1)))
  user_quo <- as.expression(quote(ff(ab = 1,abc = 1)))
  pmatch_FALSE <- grade_code(allow_partial_matching = FALSE,
             grader_args = list(
               user_quo = user_quo, 
               solution_quo = solution_quo
             ))
  
  expect_true(pmatch_FALSE$correct)
  
  
  solution_quo <- as.expression(quote(ff(ab = 1,abc = 1)))
  user_quo <- as.expression(quote(ff(abc = 1,ab = 1)))
  pmatch_FALSE <- grade_code(allow_partial_matching = FALSE,
             grader_args = list(
               user_quo = user_quo, 
               solution_quo = solution_quo
             ))
  
  expect_true(pmatch_FALSE$correct)
  
  
  

  
})