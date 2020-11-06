context("Check Result Condi")

test_that("Comparing dataframes, testing for null env", {
  source(testthat::test_path("billboard.R"))

  # check that the results are the same
  testthat::expect_equal(billboard_user, billboard_solution)

  expect_grade_this(
    expr = {
      pass_if_equal(billboard_solution, "This is a correct message")
      fail()
    },
    user_code = deparse_to_string(billboard_user),
    solution_code = deparse_to_string(billboard_solution),
    is_correct = TRUE,
    msg = "This is a correct message"
  )
})
