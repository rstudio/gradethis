test_that("gradethis_setup() sets `grading_problem.type` and `grading_problem.message`", {
  with_options(
    list(
      gradethis.grading_problem.type = NULL,
      gradethis.grading_problem.message = NULL
    ), {
      gradethis_setup(
        grading_problem.type = "info", 
        grading_problem.message = "TEST PASS"
      )
      expect_equal(gradethis_settings$grading_problem.type(), "info")
      expect_equal(gradethis_settings$grading_problem.message(), "TEST PASS")
    }
  )
})

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
