expect_condi <- function(x) {
    checkmate::expect_names(names(x), identical.to = c("x", "message", "correct", "type"))
    checkmate::expect_character(x$message, null.ok = TRUE)
    checkmate::expect_logical(x$correct, null.ok = FALSE, len = 1)
    checkmate::expect_choice(x$type, choices = c("formula", "function", "value"))
    checkmate::expect_class(x, "grader_condition")
}

expect_condi_correct <- function(x, message = NULL) {
    expect_condi(x)
    expect_equal(x$message, message)
    expect_true(x$correct)
}

expect_condi_error <- function(x, message = NULL) {
    expect_condi(x)
    expect_equal(x$message, message)
    expect_false(x$correct)
}

grader_args <- list(solution_quo = quote(5))
learnr_args <- list(envir_prep = new.env())
condi_formula_t <- condi(~ .result == 5, message = "my correct message", correct = TRUE)
condi_formula_f <- condi(~ .result == 1, message = "my error message", correct = FALSE)

context("Check condi")

test_that("Check condi", {
    expect_condi(condi_formula_t)
    expect_condi(condi_formula_f)

    expect_condi_correct(condi_formula_t, "my correct message")
    expect_condi_error(condi_formula_f, "my error message")
})

context("Check evaluate_condi")

test_that("Condi switch statement formula", {
    expect_true(
        evaluate_condi(condi_formula_t, grader_args, learnr_args)
    )

    expect_false(
        evaluate_condi(condi_formula_f, grader_args, learnr_args)
    )
})

context("Check condi formula")

test_that("Condi formula", {
    expect_true(
        evaluate_condi_formula(~ .result == 5, user_answer = 5, env = new.env())
    )

    expect_true(
        evaluate_condi_formula(~ .result == 5, user_answer = grader_args$solution_quo, env = learnr_args$envir_prep) # nolint
    )

    expect_true(
        evaluate_condi_formula(~ . == 5, user_answer = 5, env = new.env())
    )

    expect_false(
        evaluate_condi_formula(~ .result == 5, user_answer = 4, env = new.env())
    )

    expect_false(
        evaluate_condi_formula(~ . == 5, user_answer = 4, env = new.env())
    )
})
