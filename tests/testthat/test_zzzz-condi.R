last_value <- 5
envir_prep <- new.env()
envir_result <- new.env(parent = envir_prep)

condi_formula_t <- condition(~ .result == 5, message = "my correct message", correct = TRUE)
condi_formula_f <- condition(~ .result == 1, message = "my error message", correct = FALSE)

test_that("Check condi", {
  expect_condi(condi_formula_t)
  expect_condi(condi_formula_f)

  expect_condi_correct(condi_formula_t, "my correct message")
  expect_condi_error(condi_formula_f, "my error message")
})

test_that("Condi switch statement formula", {
  expect_equal(
    evaluate_condition(
      condi_formula_t,
      last_value = last_value,
      env = envir_prep
    ),
    legacy_graded(correct = TRUE, message = "my correct message")
  )

  expect_null(
    evaluate_condition(
      condi_formula_f,
      last_value = last_value,
      env = envir_prep
    )
  )
})

test_that("Condi formula", {
  expect_true(
    evaluate_condi_formula(~ .result == 5, user_answer = last_value, env = envir_prep)
  )

  expect_true(
    evaluate_condi_formula(~ .result == 5, user_answer = last_value, env = envir_prep)
  )

  expect_true(
    evaluate_condi_formula(~ . == 5, user_answer = last_value, env = envir_prep)
  )

  expect_false(
    evaluate_condi_formula(~ .result == 5, user_answer = 4, env = envir_prep)
  )

  expect_false(
    evaluate_condi_formula(~ . == 5, user_answer = 4, env = envir_prep)
  )
})
