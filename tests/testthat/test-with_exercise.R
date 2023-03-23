test_that("with_exercise()", {
  exercise <- mock_this_exercise(.user_code = "2", .solution_code = "1 + 1")

  expect_correct(with_exercise(exercise, pass_if_equal()))
  expect_wrong(with_exercise(exercise, fail_if_code_feedback()))
})
