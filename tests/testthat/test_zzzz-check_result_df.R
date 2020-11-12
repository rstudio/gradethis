context("Legacy Check Result Condi")

test_that("Comparing dataframes, testing for null env", {
  source(testthat::test_path("billboard.R"))

  # check that the results are the same
  testthat::expect_equal(billboard_user, billboard_solution)

  expect_grade_result(
    pass_if(~ identical(.result, billboard_solution), "This is a correct message"),
    last_value = billboard_user,
    is_correct = TRUE,
    msg = "This is a correct message"
  )
})
