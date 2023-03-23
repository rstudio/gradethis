test_that("Provide a passing solution. Give the students a fighting chance!", {
  testthat::expect_error(
    grade_result()
  )

  expect_grade_result(
    fail_if(~ .result == 5, "You were supposed to do this other thing!"),
    default_message = "My custom message",
    glue_correct = "{.message}",
    last_value = 6,
    is_correct = TRUE,
    msg = "My custom message"
  )

  expect_grade_result(
    fail_if(~ .result == 5, "You were supposed to do this other thing!"),
    fail_if(~ .result == 10, "You were supposed to do this other thing!"),
    last_value = 6,
    is_correct = TRUE
  )
})

test_that("Spots differences in atomics -- formula", {

  expect_grade_result(
    pass_if(~ .result == 5, "This is a correct message"),
    last_value = 5,
    is_correct = TRUE
  )

  expect_grade_result(
    pass_if(~ .result == 5, "This is a wrong answer!"),
    last_value = 100000,
    is_correct = FALSE
  )

  expect_grade_result(
    fail_if(~ .result == 5, "You were supposed to do this other thing!"),
    pass_if(~ TRUE, "should never reach here"),
    last_value = 5,
    is_correct = FALSE
  )
})

test_that("Spots differences in atomics -- function", {

  expect_grade_result(
    pass_if(function(x) x == 5, "This is a correct message"),
    last_value = 5,
    is_correct = TRUE
  )


  expect_grade_result(
    pass_if(function(x) x == 5, "This is a wrong answer!"),
    last_value = 100000,
    is_correct = FALSE
  )

  expect_grade_result(
    fail_if(function(x) x == 5, "You were supposed to do this other thing!"),
    pass_if(~ TRUE, "should never reach here"),
    last_value = 5,
    is_correct = FALSE
  )
})

test_that("Spots differences in atomics -- value", {

  expect_grade_result(
    pass_if(5, "This is a correct message"),
    last_value = 5,
    is_correct = TRUE
  )

  expect_grade_result(
    pass_if(5, "This is a wrong answer!"),
    last_value = 100000,
    is_correct = FALSE
  )

  expect_grade_result(
    fail_if(5, "You were supposed to do this other thing!"),
    pass_if(~ TRUE, "should never reach here"),
    last_value = 5,
    is_correct = FALSE
  )
})
