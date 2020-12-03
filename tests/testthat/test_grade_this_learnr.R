context("Check grade learnr")

test_that("prep environment is used", {
  expect_grade_learnr(
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
  expect_grade_learnr(
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
    {
      expect_grade_learnr(
        is_correct = FALSE,
        msg = "test parse error",
        user_code = "4 +"
      )
    }
  )
})

test_that("length 0 user code", {
  expect_grade_learnr(
    is_correct = FALSE,
    msg = "I didn't receive your code. Did you write any?",
    msg_type = "info",
    user_code = ""
  )
})

test_that("length 0 solution code", {
  expect_grade_learnr(
    is_correct = FALSE,
    msg = "A problem occurred with your teacher's grading code. Defaulting to _incorrect_",
    msg_type = "error",
    user_code = "1",
    check_code = ".solution; grade_this_code()",
    solution_code = ""
  )
})


test_that("pass / fail in check chunk are caught", {
  testthat::expect_message(
    expect_grade_learnr(
      is_correct = FALSE,
      msg = "A problem occurred with your teacher's grading code. Defaulting to _incorrect_",
      user_code = "1",
      solution_code = "1",
      check_code = "pass()"
    ),
    "Prematurely graded"
  )
  testthat::expect_message(
    expect_grade_learnr(
      is_correct = FALSE,
      msg = "A problem occurred with your teacher's grading code. Defaulting to _incorrect_",
      user_code = "1",
      solution_code = "1",
      check_code = "fail()"
    ),
    "Prematurely graded"
  )
})

test_that("check parsing error is caught", {
  testthat::expect_message(
    expect_grade_learnr(
      is_correct = FALSE,
      msg = "A problem occurred with your teacher's grading code. Defaulting to _incorrect_",
      user_code = "1",
      solution_code = "1",
      check_code = "4 +"
    ),
    "Error while executing checking `test-check` chunk: "
  )
})

test_that("return value is a function of 1 argument", {
  testthat::expect_message(
    expect_grade_learnr(
      is_correct = FALSE,
      msg = "A problem occurred with your teacher's grading code. Defaulting to _incorrect_",
      user_code = "1",
      solution_code = "1",
      check_code = "1"
    ),
    "chunk did not return a function (such as `grade_this`) that accepts 1 argument", fixed = TRUE
  )
  testthat::expect_message(
    expect_grade_learnr(
      is_correct = FALSE,
      msg = "A problem occurred with your teacher's grading code. Defaulting to _incorrect_",
      user_code = "1",
      solution_code = "1",
      check_code = "Sys.time"
    ),
    "chunk did not return a function (such as `grade_this`) that accepts 1 argument", fixed = TRUE
  )

  expect_grade_learnr(
    is_correct = TRUE,
    msg = "test pass",
    user_code = "1",
    solution_code = "1",
    check_code = "function(...) pass('test pass')"
  )
})

test_that("a grade is given", {
  testthat::expect_message(
    expect_grade_learnr(
      is_correct = FALSE,
      msg = "A problem occurred with your teacher's grading code. Defaulting to _incorrect_",
      user_code = "1",
      solution_code = "1",
      check_code = "function(...) NULL"
    ),
    "chunk did not mark an answer as correct or incorrect", fixed = TRUE
  )
  expect_grade_learnr(
    is_correct = TRUE,
    msg = "test pass",
    user_code = "1",
    solution_code = "1",
    check_code = "function(...) pass('test pass')"
  )
  expect_grade_learnr(
    is_correct = FALSE,
    msg = "test fail",
    user_code = "1",
    solution_code = "1",
    check_code = "function(...) fail('test fail')"
  )
  expect_grade_learnr(
    is_correct = FALSE,
    msg = "boom",
    user_code = "1",
    solution_code = "1",
    check_code = "function(...) stop('boom')"
  )
})
