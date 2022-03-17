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

test_that("grade_this() can find things in the parent of the check env", {
  # inspired by tblcheck::grade_this_table()
  ex <- mock_this_exercise(
    .user_code = "from_user <- TRUE",
    setup_exercise = "from_setup <- TRUE"
  )

  new_parent <- rlang::new_environment(
    list(from_parent = TRUE),
    parent = rlang::env_parent(ex)
  )
  rlang::env_poke_parent(ex, new_parent)

  found <- grade_this(
    pass(data = list(
      from_parent = from_parent,
      from_setup = from_setup,
      from_user = .envir_result$from_user
    ))
  )(ex)$data

  expect_true(found$from_parent)
  expect_true(found$from_setup)
  expect_true(found$from_user)
})
