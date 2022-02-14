test_that("grade_this() can find objects in the original env where it was defined", {
  from_calling_env <- "CALLER"
  grader <- grade_this({
    return(from_calling_env)
  })

  expect_equal(
    grader(mock_this_exercise(1)),
    "CALLER"
  )
})

test_that("grade_this() can return early", {
  expect_null(
    grade_this({
      return()
    })(mock_this_exercise(1))
  )
})
