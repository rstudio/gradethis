test_that("pass_if_equal() finds .result and .solution automatically", {
  env <- new.env()
  
  # missing .result
  pass1 <- testthat::expect_message(
    pass_if_equal(env = env), ".result", fixed = TRUE
  )
  expect_graded(
    pass1,
    is_correct = FALSE,
    msg = "problem occurred"
  )
  
  # missing .solution
  eval(quote(.result <- 12), envir = env)
  pass2 <- testthat::expect_message(
    pass_if_equal(env = env), ".solution", fixed = TRUE
  )
  expect_graded(
    pass2,
    is_correct = FALSE,
    msg = "problem occurred"
  )
  
  # Missing .solution but comparison value provided
  expect_silent(pass_if_equal(0, env = env))
  expect_condition(pass_if_equal(12, env = env))
  expect_match(pass_if_equal(12, env = env, message = "YES")$message, "YES", fixed = TRUE)
  
  # Has solution (not equal, doesn't pass)
  eval(quote(.solution <- 0), envir = env)
  expect_silent(pass_if_equal(env = env))
  
  # Has solution (equal, does pass)
  eval(quote(.solution <- 12), envir = env)
  expect_condition(pass_if_equal(env = env))
  expect_match(pass_if_equal(env = env, message = "YES")$message, "YES", fixed = TRUE)
})

test_that("fail_if_equal() finds .result", {
  env <- new.env()
  
  # missing .result
  fail1 <- testthat::expect_message(
    fail_if_equal(env = env), ".result", fixed = TRUE
  )
  expect_graded(
    fail1,
    is_correct = FALSE,
    msg = "problem occurred"
  )
  
  # Has .result (not equal, doesn't fail)
  eval(quote(.result <- 12), envir = env)
  expect_silent(fail_if_equal(0, env = env))
  # Has .result (equal, does fail)
  expect_condition(fail_if_equal(12, env = env))
  expect_match(fail_if_equal(12, env = env, message = "YES")$message, "YES", fixed = TRUE)
})

test_that("pass_if_equal() in grade_this()", {
  grader <- grade_this({
    pass_if_equal(message = "YES")
    fail("NO")
  })
  
  correct <- grader(mock_this_exercise(42, 42))
  expect_s3_class(correct, "gradethis_graded")
  expect_match(correct$message, "YES", fixed = TRUE)
  expect_true(correct$correct)
  
  incorrect <- grader(mock_this_exercise(42, 40))
  expect_s3_class(incorrect, "gradethis_graded")
  expect_match(incorrect$message, "NO", fixed = TRUE)
  expect_false(incorrect$correct)
  
  bad <- grader(mock_this_exercise(42))
  expect_s3_class(bad, "gradethis_graded")
  expect_match(bad$message, "No solution is provided")
  expect_false(bad$correct)
  
  missing_result <- testthat::expect_message(
    grader(list(.user_code = "12")), ".result", fixed = TRUE
  )
  expect_s3_class(missing_result, "gradethis_graded")
  expect_match(missing_result$message, "problem occurred", fixed = TRUE)
  expect_false(missing_result$correct)
  
  missing_solution <- testthat::expect_message(
    grader(list(.result = 12)), ".solution", fixed = TRUE
  )
  expect_s3_class(missing_solution, "gradethis_graded")
  expect_match(missing_solution$message, "problem occurred", fixed = TRUE)
  expect_false(missing_solution$correct)
})

test_that("fail_if_equal() in grade_this()", {
  grader <- grade_this({
    fail_if_equal(40, message = "NO")
    pass("YES")
  })
  
  correct <- grader(mock_this_exercise(42, 42))
  expect_s3_class(correct, "gradethis_graded")
  expect_match(correct$message, "YES", fixed = TRUE)
  expect_true(correct$correct)
  
  incorrect <- grader(mock_this_exercise(40, 42))
  expect_s3_class(incorrect, "gradethis_graded")
  expect_match(incorrect$message, "NO", fixed = TRUE)
  expect_false(incorrect$correct)
  
  missing_result <- testthat::expect_message(
    grader(list(.user_code = "12")), ".result", fixed = TRUE
  )
  expect_s3_class(missing_result, "gradethis_graded")
  expect_match(missing_result$message, "problem occurred", fixed = TRUE)
  expect_false(missing_result$correct)
})

test_that("graded() returns correct, incorrect, neutral", {
  # correct
  expect_graded(
    graded(TRUE, I("test")),
    TRUE,
    "test"
  )
  # Allows additional arguments or data in the graded condition
  correct <- expect_graded(
    graded(TRUE, I("test"), type = "info", location = "prepend"),
    TRUE,
    "test"
  )
  expect_equal(correct$type, "info")
  expect_equal(correct$location, "prepend")
  
  # incorrect
  expect_graded(graded(FALSE, I("test")), FALSE, "test")
  incorrect <- expect_graded(
    graded(FALSE, I("test"), type = "warning", location = "replace"),
    FALSE,
    "test"
  )
  expect_equal(incorrect$type, "warning")
  expect_equal(incorrect$location, "replace")
  
  # neutral
  expect_graded(graded(logical(), I("test")), logical(), "test")
  neutral <- expect_graded(
    graded(logical(), I("test"), type = "custom", location = "append"),
    logical(),
    "test"
  )
  expect_equal(neutral$type, "custom")
  expect_equal(neutral$location, "append")
  
  expect_error(graded("boom", I("bad")))
})
