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
  lifecycle::expect_deprecated(gradethis_setup(fail_code_feedback = FALSE))
  lifecycle::expect_deprecated(grade_this(fail(), fail_code_feedback = FALSE))
})

test_that("space_before and space_after in maybe_code_feedback() are deprecated", {
  user <- "runif()"
  solution <- "rnorm()"
  
  lifecycle::expect_deprecated(maybe_code_feedback(user, solution, space_before = TRUE))
  lifecycle::expect_deprecated(maybe_code_feedback(user, solution, space_after = TRUE))
  
  lifecycle::expect_deprecated(
    expect_equal(
      maybe_code_feedback(user, solution, space_before = TRUE),
      maybe_code_feedback(user, solution, before = " ")
    )
  )
  
  lifecycle::expect_deprecated(
    expect_equal(
      maybe_code_feedback(user, solution, space_after = TRUE),
      maybe_code_feedback(user, solution, after = " ")
    )
  )
  
  lifecycle::expect_deprecated(
    expect_equal(
      maybe_code_feedback(user, solution, before = "\n", space_before = TRUE),
      maybe_code_feedback(user, solution, before = "\n")
    )
  )
  
  lifecycle::expect_deprecated(
    expect_equal(
      maybe_code_feedback(user, solution, after = "\n", space_after = TRUE),
      maybe_code_feedback(user, solution, after = "\n")
    )
  )
})
