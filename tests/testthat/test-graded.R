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

test_that("graded() ensures that ... are empty", {
  expect_error(graded(TRUE, "foo", arg = "boom!"))
  expect_error(graded(TRUE, "foo", "boom!"))
})

test_that("pass_if() and fail_if() use default pass/fail message in grade_this()", {
  with_gradethis_setup(
    pass = "TEST PASSED",
    fail = "TEST FAILED",
    expect_grade_this({
      pass_if(.result < 5)
      fail_if(.result >= 5)
      fail("TEST FAILED")
    },
      user_code = "2",
      is_correct = TRUE,
      msg = "TEST PASSED"
    )
  )
  
  with_gradethis_setup(
    pass = "TEST FAILED",
    fail = "TEST PASSED",
    expect_grade_this({
      pass_if(.result < 5)
      fail_if(.result >= 5)
      fail("TEST FAILED")
    },
      user_code = "6",
      is_correct = FALSE,
      msg = "TEST PASSED"
    )
  )
  
  with_gradethis_setup(
    pass = "TEST FAILED",
    fail = "TEST PASSED.{maybe_code_feedback()}",
    expect_match(
      expect_grade_this({
        pass_if(.result < 5)
        fail_if(.result >= 5)
        fail("TEST FAILED")
      },
        user_code = "6",
        solution_code = "2",
        is_correct = FALSE,
      )$message,
      "TEST PASSED\\. I expected"
    )
  )
})

test_that("grade_if_equal() edge cases with diffobj::ses()", {
  result <- c(39.6, 40.1, 35, 42, 34.5, 41.4, 39, 40.6, 36.5, 37.6, 35.7, 
    41.3, 37.6, 41.1, 36.4, 41.6, 35.5, 41.1, 35.9, 41.8, 33.5, 39.7, 
    39.6, 45.8, 35.5, 42.8, 40.9, 37.2, 36.2, 42.1, 34.6, 42.9, 36.7, 
    35.1, 37.3, 41.3, 36.3, 36.9, 38.3, 38.9, 35.7, 41.1, 34, 39.6, 
    36.2, 40.8, 38.1, 40.3, 33.1, 43.2, 49.1, 48.4, 42.6, 44.4, 44, 
    48.7, 42.7, 49.6, 45.3, 49.6, 50.5, 43.6, 45.5, 50.5, 44.9, 45.2, 
    46.6, 48.5, 45.1, 50.1, 46.5, 45, 43.8, 45.5, 43.2, 50.4, 45.3, 
    46.2, 45.7, 54.3, 45.8, 49.8, 46.2, 49.5, 43.5, 50.7, 47.7, 46.4, 
    48.2, 46.5, 46.4, 48.6, 47.5, 51.1, 45.2, 45.2, 50.5, 49.5, 46.4, 
    52.8, 40.9, 54.2, 42.5, 51, 49.7, 47.5, 47.6, 52, 46.9, 53.5, 
    49, 46.2, 50.9, 45.5)
  
  solution <- c(39.1, 39.5, 40.3, NA, 36.7, 39.3, 38.9, 39.2, 34.1, 42, 37.8, 
    37.8, 41.1, 38.6, 34.6, 36.6, 38.7, 42.5, 34.4, 46, 37.8, 37.7, 
    35.9, 38.2, 38.8, 35.3, 40.6, 40.5, 37.9, 40.5, 39.5, 37.2, 39.5, 
    40.9, 36.4, 39.2, 38.8, 42.2, 37.6, 39.8, 36.5, 40.8, 36, 44.1, 
    37, 39.6, 41.1, 37.5, 36, 42.3, 46.1, 50, 48.7, 50, 47.6, 46.5, 
    45.4, 46.7, 43.3, 46.8, 40.9, 49, 45.5, 48.4, 45.8, 49.3, 42, 
    49.2, 46.2, 48.7, 50.2, 45.1, 46.5, 46.3, 42.9, 46.1, 44.5, 47.8, 
    48.2, 50, 47.3, 42.8, 45.1, 59.6, 49.6, 50.5, 50.5, 50.1, 50.4, 
    46.2, 54.3, 49.8, 49.5, 50.7, 46.4, 48.2, 48.6, 45.2, 52.5, 50, 
    50.8, 52.1, 52.2, 49.5, 50.8, 46.9, 51.1, 55.9, 49.1, 49.8, 51.5, 
    55.1, 48.8, 50.4, 46.5, 50, 51.3, 45.4, 52.7, 45.2, 46.1, 51.3, 
    46, 51.3, 46.6, 51.7, 47, 52, 45.9, 50.5, 50.3, 58, 46.4, 49.2, 
    42.4, 48.5, 43.2, 50.6, 46.7, 52)
  
  # Logic Error: Exceeded buffer for finding fake snake; contact maintainer.
  expect_null(
    grade_if_equal(result, solution, message = "TEST FAILED", FALSE)
  )
  
  a <- 1:20
  b <- 10:500
  # Internal Error: reached theoretically unreachable branch 2, contact maintainer.
  expect_null(
    grade_if_equal(a, b, message = "TEST_FAILED", FALSE)
  )
})

test_that("praise argument works with passing grades", {
  with_seed(
    seed = 33,
    expect_graded(
      pass("xxx", praise = TRUE),
      is_correct = TRUE,
      msg = paste(with_seed(33, random_praise()), "xxx")
    )
  )
  
  with_seed(
    seed = 12,
    expect_graded(
      pass_if_equal(x = 1, y = 1, message = "xxx", praise = TRUE),
      is_correct = TRUE,
      msg = paste(with_seed(12, random_praise()), "xxx")
    )
  )
  
  with_options(
    list(gradethis.pass.praise = TRUE),
    with_seed(
      seed = 99,
      expect_graded(
        pass(message = "xxx"),
        is_correct = TRUE,
        msg = paste(with_seed(99, random_praise()), "xxx")
      )
    )
  )
  
  gradethis_env <- rlang::env(".__gradethis_check_env" = TRUE)
  
  # only one random_praise(), praise = TRUE wins
  with_seed(
    seed = 84,
    expect_graded(
      pass_if(TRUE, message = "{random_praise()}", praise = TRUE, env = gradethis_env),
      is_correct = TRUE,
      msg = with_seed(84, random_praise())
    )
  )
})

test_that("encourage argument works with failing grades", {
  with_seed(
    seed = 33,
    expect_graded(
      fail("xxx", encourage = TRUE),
      is_correct = FALSE,
      msg = paste("xxx", with_seed(33, random_encouragement()))
    )
  )
  
  with_seed(
    seed = 12,
    expect_graded(
      fail_if_equal(x = 1, y = 1, message = "xxx", encourage = TRUE),
      is_correct = FALSE,
      msg = paste("xxx", with_seed(12, random_encouragement()))
    )
  )
  
  with_options(
    list(gradethis.fail.encourage = TRUE),
    with_seed(
      seed = 99,
      expect_graded(
        fail(message = "xxx"),
        is_correct = FALSE,
        msg = paste("xxx", with_seed(99, random_encouragement()))
      )
    )
  )
  
  gradethis_env <- rlang::env(".__gradethis_check_env" = TRUE)
  
  # only one random_encouragement(), encourage = TRUE wins
  with_seed(
    seed = 84,
    expect_graded(
      fail_if(TRUE, message = "{random_encouragement()}", encourage = TRUE, env = gradethis_env),
      is_correct = FALSE,
      msg = with_seed(84, random_encouragement())
    )
  )
})
