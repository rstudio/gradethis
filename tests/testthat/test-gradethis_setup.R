test_that("gradethis_setup() issues warning for invalid `grading_problem.type`", {
  with_options(list(gradethis.grading_problem.type = NULL), {
    testthat::expect_message(
      gradethis_setup(grading_problem.type = "bad"), 
      'Defaulting to "warning"'
    )
    expect_equal(
      gradethis_settings$grading_problem.type(), 
      gradethis_default_options$grading_problem.type
    )
  })
  
  with_options(list(gradethis.grading_problem.type = "PASS"), {
    testthat::expect_message(
      gradethis_setup(grading_problem.type = "bad"), 
      'Defaulting to "warning"'
    )
  })
})
