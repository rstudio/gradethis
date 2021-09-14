context("Check grade learnr")

test_that("prep environment is used", {
  expect_exercise_checker(
    is_correct = TRUE,
    msg = "yes. you did it. Extra!",
    user_code = "4",
    prep_code = "
      extra <- 'Extra!'
    ",
    check_code = "
      grade_this({
        pass_if_equal(4, 'yes. you did it. {extra}')
      })
    "
  )
})

test_that("check environment is used", {
  expect_exercise_checker(
    is_correct = TRUE,
    msg = "yes. you did it. Extra!",
    user_code = "4",
    check_code = "
      extra <- 'Extra!'
      grade_this({
        pass_if_equal(4, 'yes. you did it. {extra}')
      })
    "
  )
})

test_that("parser error is used", {
  with_options(
    list(
      exercise.parse.error = function(check_obj) {
        expect_true(".error" %in% names(check_obj))
        fail("test parse error")
      }
    ),
    expect_exercise_checker(
      is_correct = FALSE,
      msg = "test parse error",
      user_code = "4 +"
    )
  )
})

test_that("length 0 user code", {
  expect_exercise_checker(
    is_correct = FALSE,
    msg = "I didn't receive your code. Did you write any?",
    msg_type = "info",
    user_code = ""
  )
})

test_that("user and solution code are always length 1", {
  expect_exercise_checker(
    is_correct = TRUE,
    msg = "TEST PASSED",
    user_code = c("1", "2", "3"),
    check_code = "grade_this(if (length(.user_code) == 1) pass('TEST PASSED') else fail('TEST FAILED'))"
  )
  
  expect_exercise_checker(
    is_correct = TRUE,
    msg = "TEST PASSED",
    user_code = c("1", "2", "3"),
    solution_code= c("1", "2", "3", "4"),
    check_code = "grade_this(if (length(.solution_code) == 1) pass('TEST PASSED') else fail('TEST FAILED'))"
  )
  
  expect_exercise_checker(
    is_correct = TRUE,
    msg = "TEST PASSED",
    user_code = c("1", "2", "3"),
    solution_code= c("1", "2", "3", "4"),
    check_code = c(
      "grade_this(", 
      "if (length(.check_code) == 1)",
      "pass('TEST PASSED')", 
      "else", 
      "fail('TEST FAILED')", 
      ")"
    )
  )
})

# "A problem occurred with your teacher's grading code. Defaulting to _incorrect_."
message_feedback_grading_problem <- feedback_grading_problem()$message

test_that("length 0 solution code", {
  expect_exercise_checker(
    is_correct = logical(),
    msg = "No solution is provided for this exercise.",
    msg_type = "info",
    user_code = "1",
    check_code = "grade_this({pass(.solution)})",
    solution_code = ""
  )
})


test_that("pass / fail in check chunk are caught", {
  err <- testthat::expect_message(
    expect_exercise_checker(
      is_correct = FALSE,
      msg = message_feedback_grading_problem,
      user_code = "1",
      solution_code = "1",
      check_code = "pass()",
      error_message = "prematurely graded"
    ),
    "prematurely graded"
  )
  expect_equal(err$error$call, "pass()")
  expect_equal(err$error$label, "test-check")
  
  err <- testthat::expect_message(
    expect_exercise_checker(
      is_correct = FALSE,
      msg = message_feedback_grading_problem,
      user_code = "1",
      solution_code = "1",
      check_code = "fail()",
      error_message = "prematurely graded"
    ),
    "prematurely graded"
  )
  expect_equal(err$error$call, "fail()")
  expect_equal(err$error$label, "test-check")
})

test_that("check parsing error is caught", {
  err <- testthat::expect_message(
    expect_exercise_checker(
      is_correct = FALSE,
      msg = message_feedback_grading_problem,
      user_code = "1",
      solution_code = "1",
      check_code = "4 +",
      error_message = "unexpected end of input"
    ),
    "Error while checking `test-check` chunk: "
  )
  
  expect_equal(err$error$call, "4 +")
  expect_equal(err$error$label, "test-check")
})

test_that("return value is a function of 1 argument", {
  err <- testthat::expect_message(
    expect_exercise_checker(
      is_correct = FALSE,
      msg = message_feedback_grading_problem,
      user_code = "1",
      solution_code = "1",
      check_code = "1",
      error_message = "chunk did not return a function"
    ),
    "chunk did not return a function (such as `grade_this`) that accepts 1 argument", fixed = TRUE
  )
  
  expect_equal(err$error$call, "1")
  expect_equal(err$error$label, "test-check")
  
  err <- testthat::expect_message(
    expect_exercise_checker(
      is_correct = FALSE,
      msg = message_feedback_grading_problem,
      user_code = "1",
      solution_code = "1",
      check_code = "Sys.time",
      error_message = "chunk did not return a function"
    ),
    "chunk did not return a function (such as `grade_this`) that accepts 1 argument", fixed = TRUE
  )
  expect_equal(err$error$call, "Sys.time")
  expect_equal(err$error$label, "test-check")

  expect_exercise_checker(
    is_correct = TRUE,
    msg = "test pass",
    user_code = "1",
    solution_code = "1",
    check_code = "function(...) pass('test pass')"
  )
})

test_that("a grade is given", {
  err <- testthat::expect_message(
    expect_exercise_checker(
      is_correct = FALSE,
      msg = message_feedback_grading_problem,
      user_code = "1",
      solution_code = "1",
      check_code = "function(...) NULL",
      error_message = "chunk did not mark an answer as correct or incorrect"
    ),
    "chunk did not mark an answer as correct or incorrect", fixed = TRUE
  )
  expect_equal(err$error$call, "1")
  expect_equal(err$error$label, "test-check")
  
  expect_exercise_checker(
    is_correct = TRUE,
    msg = "test pass",
    user_code = "1",
    solution_code = "1",
    check_code = "function(...) pass('test pass')"
  )
  expect_exercise_checker(
    is_correct = FALSE,
    msg = "test fail",
    user_code = "1",
    solution_code = "1",
    check_code = "function(...) fail('test fail')"
  )
  expect_exercise_checker(
    is_correct = FALSE,
    msg = "boom",
    user_code = "1",
    solution_code = "1",
    check_code = "function(...) stop('boom')"
  )
})

test_that("pass_if() and fail_if() work in grade_this()", {
  expect_exercise_checker(
    is_correct = TRUE,
    msg = "1 + 1 is right!",
    user_code = "1 + 1",
    solution_code = "1 + 1",
    check_code = "grade_this(pass_if(.result == .solution, '{.user_code} is right!'))"
  )
  
  expect_exercise_checker(
    is_correct = FALSE,
    msg = "1 + 3 is wrong!",
    user_code = "1 + 3",
    solution_code = "1 + 1",
    check_code = "grade_this(fail_if(.result != .solution, '{.user_code} is wrong!'))"
  )
  
  expect_warning(
    expect_exercise_checker(
      is_correct = logical(),
      msg = I("problem"),
      user_code = "1 + 3",
      solution_code = "1 + 1",
      check_code = "grade_this(fail_if(~ .result != .solution, '{.user_code} is meh'))",
      msg_type = "warning",
      msg_fixed = TRUE
    ),
    # fail_if() doesn't accept...
    "functions or formulas"
  )
  
  err <- expect_warning(
    expect_exercise_checker(
      is_correct = logical(),
      msg = I("problem"),
      user_code = "1 + 1",
      solution_code = "1 + 1",
      check_code = "grade_this(pass_if(~ .result != .solution, '{.user_code} is meh'))",
      msg_type = "warning",
      msg_fixed = TRUE
    ),
    # pass_if() doesn't accept...
    "functions or formulas"
  )
  expect_match(err$error$message, "does not accept functions or formulas")
})

