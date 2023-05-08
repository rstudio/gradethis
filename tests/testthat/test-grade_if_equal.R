test_that("pass_if_equal() finds .result and .solution automatically", {
  env <- new.env()

  # missing .result
  pass1 <-
    testthat::expect_message(
      expect_graded(
        pass_if_equal(env = env),
        is_correct = logical(0),
        msg = "problem occurred"
      ),
      ".result",
      fixed = TRUE
    )

  # missing .solution
  eval(quote(.result <- 12), envir = env)
  pass2 <- testthat::expect_message(
    expect_graded(
      pass_if_equal(env = env),
      is_correct = logical(0),
      msg = "problem occurred"
    ),
    ".solution", fixed = TRUE
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
    expect_graded(
      fail_if_equal(env = env),
      is_correct = logical(),
      msg = "problem occurred"
    ),
    regexp = ".result",
    fixed = TRUE
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

  # correct
  expect_graded(
    grader(mock_this_exercise(42, 42)),
    is_correct = TRUE,
    msg = "YES"
  )

  # incorrect
  expect_graded(
    grader(mock_this_exercise(42, 40)),
    is_correct = FALSE,
    msg = "NO"
  )

  # bad: no solution
  expect_graded(
    grader(mock_this_exercise(42)),
    is_correct = logical(),
    msg = "No solution is provided"
  )


  # bad: no .result object
  testthat::expect_message(
    expect_graded(
      grader(list(.user_code = "12")),
      is_correct = logical(),
      msg = "problem occurred"
    ),
    regexp = ".result",
    fixed = TRUE
  )

  # bad: no .solution object
  testthat::expect_message(
    expect_graded(
      grader(list(.result = "12")),
      is_correct = logical(),
      msg = "problem occurred"
    ),
    regexp = ".solution",
    fixed = TRUE
  )
})

test_that("pass_if_equal() with multiple solutions", {
  grader <- grade_this({
    pass_if_equal(y = .solution_all, message = "YES {.solution_code} - {.solution_label} - {.solution}")
    fail("NO")
  })

  solution_code <-
    "# first ----
1
# alt ----
2
# alt ----
3"

  # correct
  grade_first <- expect_graded(
    grader(mock_this_exercise(1, !!solution_code)),
    is_correct = TRUE,
    msg = "YES 1 - first - 1"
  )
  expect_equal(grade_first$solution_label, "first")
  expect_equal(grade_first$solution_index, 1L)

  grade_alt <- expect_graded(
    grader(mock_this_exercise(2, !!solution_code)),
    is_correct = TRUE,
    msg = "YES 2 - alt - 2"
  )
  expect_equal(grade_alt$solution_label, "alt")
  expect_equal(grade_alt$solution_index, 2L)

  grade_alt_1 <- expect_graded(
    grader(mock_this_exercise(3, !!solution_code)),
    is_correct = TRUE,
    msg = "YES 3 - alt - 3"
  )
  expect_equal(grade_alt_1$solution_label, "alt")
  expect_equal(grade_alt_1$solution_index, 3L)

  # incorrect
  expect_graded(
    grader(mock_this_exercise(4, !!solution_code)),
    is_correct = FALSE,
    msg = "NO"
  )

  expect_graded(
    grader(mock_this_exercise("c(1, 2, 3)", !!solution_code)),
    is_correct = FALSE,
    msg = "NO"
  )

  expect_graded(
    grader(mock_this_exercise("list(1, 2, 3)", !!solution_code)),
    is_correct = FALSE,
    msg = "NO"
  )

  expect_graded(
    grader(mock_this_exercise(
      "gradethis:::gradethis_solutions(one = 1, two = 2, three = 3)",
      !!solution_code
    )),
    is_correct = FALSE,
    msg = "NO"
  )
})

test_that("fail_if_equal() in grade_this()", {
  grader <- grade_this({
    fail_if_equal(40, message = "NO")
    pass("YES")
  })

  expect_graded(
    grader(mock_this_exercise(42, 42)),
    msg = "YES",
    is_correct = TRUE
  )

  expect_graded(
    grader(mock_this_exercise(40, 42)),
    msg = "NO",
    is_correct = FALSE
  )

  # bad: no .result object
  testthat::expect_message(
    expect_graded(
      grader(list(.user_code = "12")),
      is_correct = logical(),
      msg = "problem occurred"
    ),
    regexp = ".result",
    fixed = TRUE
  )
})

test_that("pass_if_equal() with tolerance", {
  # pass
  expect_graded(
    pass_if_equal(y = 2, x = sqrt(2) ^ 2),
    is_correct = TRUE
  )

  # no pass
  expect_null(pass_if_equal(y = 2, x = sqrt(2) ^ 2, tolerance = 0))
})

test_that("fail_if_equal() with tolerance", {
  # fail
  expect_graded(
    fail_if_equal(y = 2, x = sqrt(2) ^ 2),
    is_correct = FALSE
  )

  # no fail
  expect_null(fail_if_equal(y = 2, x = sqrt(2) ^ 2, tolerance = 0))
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

  a <- array(1, dim = 5, dimnames = list(letters[1:5]))
  b <- array(2, dim = 5, dimnames = list(letters[1:5]))
  # names(lines) <- format(c("", row_idx), align = "right"):
  # 'names' attribute [11] must be the same length as the vector [3]
  expect_null(
    grade_if_equal(a, b, message = "TEST_FAILED", FALSE)
  )

  a <- array(1, dim = rep(5, 3), dimnames = rep(list(letters[1:5]), 3))
  b <- array(2, dim = rep(5, 3), dimnames = rep(list(letters[1:5]), 3))
  # names(lines) <- format(c("", row_idx), align = "right"):
  # 'names' attribute [11] must be the same length as the vector [3]
  expect_null(
    grade_if_equal(a, b, message = "TEST_FAILED", FALSE)
  )
})
