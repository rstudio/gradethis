context("Check Result Condi")

test_that("Provide a passing solution. Give the students a fighting chance!", {
  testthat::expect_error(
    grade_result(
      grader_args = list(),
      learnr_args = list(last_value = 5, envir_prep = new.env())
    )
  )

  testthat::expect_error(
    grade_result(
      fail_if(~ .result == 5, "You were supposed to do this other thing!"),
      grader_args = list(),
      learnr_args = list(last_value = 5, envir_prep = new.env())
    )
  )

  testthat::expect_error(
    grade_result(
      fail_if(~ .result == 5, "You were supposed to do this other thing!"),
      fail_if(~ .result == 10, "You were supposed to do this other thing!"),
      grader_args = list(),
      learnr_args = list(last_value = 5, envir_prep = new.env())
    )
  )
})

test_that("Spots differences in atomics -- formula", {

  expect_correct(
    grade_result(
        pass_if(~ .result == 5, "This is a correct message"),
        grader_args = list(),
        learnr_args = list(last_value = 5, envir_prep = new.env())
    )
  )

  expect_wrong(
    grade_result(
        pass_if(~ .result == 5, "This is a wrong answer!"),
        grader_args = list(),
        learnr_args = list(last_value = 100000, envir_prep = new.env())
    )
  )

  expect_wrong(
    grade_result(
        fail_if(~ .result == 5, "You were supposed to do this other thing!"),
        pass_if(~ TRUE, "should never reach here"),
        grader_args = list(),
        learnr_args = list(last_value = 5, envir_prep = new.env())
    )
  )
})

test_that("Spots differences in atomics -- function", {

  expect_correct(
    grade_result(
        pass_if(function(x) x == 5, "This is a correct message"),
        learnr_args = list(last_value = 5)
    )
  )

  expect_wrong(
    grade_result(
        pass_if(function(x) x == 5, "This is a wrong answer!"),
        learnr_args = list(last_value = 100000)
    )
  )

  expect_wrong(
    grade_result(
        fail_if(function(x) x == 5, "You were supposed to do this other thing!"),
        pass_if(~ TRUE, "should never reach here"),
        learnr_args = list(last_value = 5)
    )
  )
})

test_that("Spots differences in atomics -- value", {

  expect_correct(
    grade_result(
        pass_if(5, "This is a correct message"),
        learnr_args = list(last_value = 5)
    )
  )

  expect_wrong(
    grade_result(
        pass_if(5, "This is a wrong answer!"),
        learnr_args = list(last_value = 100000)
    )
  )

  expect_wrong(
    grade_result(
        fail_if(5, "You were supposed to do this other thing!"),
        pass_if(~ TRUE, "should never reach here"),
        learnr_args = list(last_value = 5)
    )
  )
})
