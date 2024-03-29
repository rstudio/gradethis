test_that("grade_conditions() is deprecated", {
  lifecycle::expect_deprecated(grade_conditions())
})

test_that("grade_feedback() is deprecated", {
  lifecycle::expect_deprecated(grade_feedback(graded(FALSE)))
})

test_that("random_encourage() is deprecated", {
  lifecycle::expect_deprecated(random_encourage())
})

test_that("grade_learnr() is deprecated", {
  lifecycle::expect_deprecated(grade_learnr())
})

test_that("fail_code_feedback= is deprecated", {
  lifecycle::expect_deprecated(
    with_gradethis_setup(
      fail_code_feedback = FALSE,
      expect_false(getOption("gradethis.maybe_code_feedback"))
    )
  )
  expect_true(getOption("gradethis.maybe_code_feedback"))
  lifecycle::expect_deprecated(grade_this(fail(), fail_code_feedback = FALSE))
})

test_that("space_before and space_after in maybe_code_feedback() are deprecated", {
  .user <- "runif()"
  .solution <- "rnorm()"

  with_gradethis_setup(
    maybe_code_feedback = TRUE,
    {
      lifecycle::expect_deprecated(maybe_code_feedback(.user, .solution, space_before = TRUE))
      lifecycle::expect_deprecated(maybe_code_feedback(.user, .solution, space_after = TRUE))

      with_options(
        list(lifecycle_verbosity = "quiet"),
        expect_equal(
          maybe_code_feedback(.user, .solution, space_before = TRUE),
          maybe_code_feedback(.user, .solution, before = " ")
        )
      )

      with_options(
        list(lifecycle_verbosity = "quiet"),
        expect_equal(
          maybe_code_feedback(.user, .solution, space_after = TRUE),
          maybe_code_feedback(.user, .solution, after = " ")
        )
      )

      with_options(
        list(lifecycle_verbosity = "quiet"),
        expect_equal(
          maybe_code_feedback(.user, .solution, before = "\n", space_before = TRUE),
          maybe_code_feedback(.user, .solution, before = "\n")
        )
      )

      with_options(
        list(lifecycle_verbosity = "quiet"),
        expect_equal(
          maybe_code_feedback(.user, .solution, after = "\n", space_after = TRUE),
          maybe_code_feedback(.user, .solution, after = "\n")
        )
      )
    }
  )
})

test_that("x is deprecated in pass_if() and fail_if()", {
  with_options(
    list(lifecycle_verbosity = "quiet"),
    expect_equal(
      pass_if(x = TRUE, message = "deprecated"),
      pass_if(TRUE, "deprecated")
    )
  )

  with_options(
    list(lifecycle_verbosity = "quiet"),
    expect_equal(
      fail_if(x = TRUE, message = "deprecated"),
      fail_if(TRUE, "deprecated")
    )
  )
})
