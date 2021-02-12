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
